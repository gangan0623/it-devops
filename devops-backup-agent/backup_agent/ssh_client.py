import logging
import re
import time

import paramiko

from .cleaner import OutputCleaner
from .validator import ConfigValidator


class SshService:
    def __init__(self, timeout=100000, connect_timeout=30000):
        self.timeout = timeout / 1000.0
        self.connect_timeout = connect_timeout / 1000.0

    def get_huawei_config(self, host, username, password):
        return self._execute_device_commands(
            host,
            username,
            password,
            _device_commands("huawei"),
            OutputCleaner.DeviceType.HUAWEI,
            ConfigValidator.DeviceType.HUAWEI,
        )

    def get_h3c_config(self, host, username, password):
        return self._execute_device_commands(
            host,
            username,
            password,
            _device_commands("h3c"),
            OutputCleaner.DeviceType.H3C,
            ConfigValidator.DeviceType.H3C,
        )

    def get_forti_config(self, host, username, password):
        return self._execute_device_commands(
            host,
            username,
            password,
            _device_commands("forti"),
            OutputCleaner.DeviceType.FORTI,
            ConfigValidator.DeviceType.FORTI,
        )

    def get_rgos_config(self, host, username, password):
        return self._execute_device_commands(
            host,
            username,
            password,
            _device_commands("rgos"),
            OutputCleaner.DeviceType.RGOS,
            ConfigValidator.DeviceType.RGOS,
        )

    def _execute_device_commands(self, host, username, password, commands, clean_type, validator_type):
        max_retries = 3
        for attempt in range(1, max_retries + 1):
            try:
                logging.info("attempt %s execute commands: %s", attempt, host)
                cleaned = self._execute_ssh_commands_structured(host, username, password, commands, clean_type)
                if ConfigValidator.is_valid(cleaned, validator_type):
                    logging.info("attempt %s success, length: %s", attempt, len(cleaned))
                    return cleaned
                logging.warning("attempt %s invalid config", attempt)
                if attempt < max_retries:
                    time.sleep(3 * attempt)
            except Exception as e:
                logging.error("attempt %s failed: %s", attempt, e)
                if attempt == max_retries:
                    raise RuntimeError(f"execute commands failed after {max_retries} retries") from e
                time.sleep(5 * attempt)
        raise RuntimeError("execute commands failed, all retries invalid")

    def _execute_ssh_commands(self, host, username, password, commands):
        client = paramiko.SSHClient()
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        try:
            client.connect(
                hostname=host,
                username=username,
                password=password,
                port=22,
                timeout=self.connect_timeout,
                auth_timeout=self.connect_timeout,
                banner_timeout=self.connect_timeout,
            )
            channel = client.invoke_shell()
            channel.settimeout(self.timeout)
            time.sleep(0.3)
            _read_available(channel)
            _wait_for_prompt(channel)

            buffer = bytearray()
            for command in commands:
                _send_command(channel, command)
                _wait_for_command_completion(channel, buffer, _is_main_command(command))

            try:
                _send_command(channel, "quit")
                time.sleep(0.8)
                _send_command(channel, "exit")
                time.sleep(0.5)
                _read_available(channel, buffer)
            except Exception:
                pass

            raw = _decode_output(bytes(buffer))
            logging.info("command sequence completed, raw length: %s", len(raw))
            return raw
        except Exception as e:
            raise RuntimeError(f"SSH connection failed: {e}") from e
        finally:
            try:
                client.close()
            except Exception:
                pass

    def _execute_ssh_commands_structured(self, host, username, password, commands, clean_type):
        client = paramiko.SSHClient()
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        try:
            client.connect(
                hostname=host,
                username=username,
                password=password,
                port=22,
                timeout=self.connect_timeout,
                auth_timeout=self.connect_timeout,
                banner_timeout=self.connect_timeout,
            )
            channel = client.invoke_shell()
            channel.settimeout(self.timeout)
            time.sleep(0.5)

            prompt = _detect_prompt_with_buffer(channel)
            skip = _skip_commands_for_type(clean_type)
            outputs = {}

            for command in commands:
                _send_command(channel, command)
                output = _read_until_prompt(channel, prompt, _command_timeout(command), require_prompt=bool(prompt))
                if command not in skip:
                    outputs[command] = output

            return _format_outputs(outputs, clean_type, prompt)
        except Exception as e:
            raise RuntimeError(f"SSH connection failed: {e}") from e
        finally:
            try:
                client.close()
            except Exception:
                pass


def _device_commands(kind):
    if kind == "huawei":
        return [
            "screen-length 0 temporary",
            "display version",
            "display device",
            "display current-configuration",
        ]
    if kind == "h3c":
        return [
            "screen-length disable",
            "display version",
            "display device",
            "display device manuinfo",
            "display current-configuration",
        ]
    if kind == "forti":
        return [
            "get system status | grep .",
            "get system ha status | grep .",
            "get hardware status | grep .",
            "show full-configuration | grep .",
        ]
    if kind == "rgos":
        return [
            "terminal length 0",
            "terminal width 0",
            "show version",
            "show running-config",
        ]
    return []


def _send_command(channel, command):
    logging.info("execute command: %s", command)
    channel.send(command + "\n")


def _is_main_command(command):
    lower = command.lower()
    return (
        "display current-configuration" in lower
        or "show running-config" in lower
        or "show configuration" in lower
        or "show full-configuration" in lower
    )


def _wait_for_prompt(channel):
    waited = 0.0
    while waited < 5.0:
        buf = _read_available(channel)
        if buf:
            text = _safe_decode(buf)
            if any(p in text for p in [">", "#", "$", "]"]):
                time.sleep(0.5)
                return
        time.sleep(0.2)
        waited += 0.2
    logging.warning("prompt not detected, continue")


def _wait_for_command_completion(channel, buffer, is_main):
    max_wait = 180.0 if is_main else 8.0
    min_wait = 2.0
    waited = 0.0
    found_return = False
    while waited < max_wait:
        time.sleep(0.5)
        waited += 0.5
        _read_available(channel, buffer)
        current_text = _safe_decode(bytes(buffer))
        if _detect_paging(current_text):
            _send_command(channel, " ")
        if _check_for_completion(current_text):
            found_return = True
        if waited >= min_wait and found_return:
            time.sleep(0.5)
            return
    logging.warning("command wait timeout: %s seconds", max_wait)


def _check_for_completion(output):
    lower = output.lower()
    if "return" in lower or ">" in lower or "#" in lower:
        return True
    lines = output.split("\n")
    if lines:
        for line in lines[-3:]:
            line = line.strip()
            if line.endswith("]") or line.endswith(">") or line.endswith("#"):
                return True
    return False


def _read_available(channel, buffer=None):
    data = bytearray()
    while channel.recv_ready():
        chunk = channel.recv(4096)
        if not chunk:
            break
        data.extend(chunk)
    if buffer is not None and data:
        buffer.extend(data)
    return bytes(data)


def _decode_output(raw):
    if raw is None:
        return ""
    try:
        text = raw.decode("utf-8")
        if not _contains_garbled(text):
            return text
    except Exception:
        pass
    try:
        return raw.decode("gbk")
    except Exception:
        return raw.decode(errors="ignore")


def _safe_decode(raw):
    try:
        return raw.decode("utf-8", errors="ignore")
    except Exception:
        try:
            return raw.decode("gbk", errors="ignore")
        except Exception:
            return ""


def _contains_garbled(text):
    if not text:
        return False
    if "????" in text:
        return True
    return bool(re.search(r"[\x00-\x08\x0B\x0C\x0E-\x1F\x7F-\x84\x86-\x9F]", text))


def _detect_prompt_with_buffer(channel):
    buf = bytearray()
    start = time.time()
    while time.time() - start < 5.0:
        if channel.recv_ready():
            buf.extend(channel.recv(65535))
        else:
            time.sleep(0.1)
        if buf:
            text = _safe_decode(bytes(buf))
            m = re.search(r"<([^>]+)>", text)
            if m:
                return f"<{m.group(1)}>"
            m = re.search(r"([^\s]+)\s*#", text)
            if m:
                return f"{m.group(1)} #"
    text = _safe_decode(bytes(buf))
    m = re.search(r"<([^>]+)>", text)
    if m:
        return f"<{m.group(1)}>"
    m = re.search(r"([^\s]+)\s*#", text)
    if m:
        return f"{m.group(1)} #"
    return None


def _read_until_prompt(channel, prompt, timeout, require_prompt=False):
    start = time.time()
    last_recv = time.time()
    data = bytearray()
    while time.time() - start < timeout:
        if channel.recv_ready():
            chunk = channel.recv(65535)
            if chunk:
                data.extend(chunk)
                last_recv = time.time()
                if prompt:
                    decoded = _safe_decode(bytes(data))
                    lines = decoded.strip().split("\n")
                    if lines:
                        last = lines[-1].strip()
                        if prompt in last or re.match(r"^<[^>]+>$", last):
                            time.sleep(0.2)
                            break
        else:
            if not require_prompt and time.time() - last_recv > 1.0:
                break
            time.sleep(0.1)
    return _safe_decode(bytes(data))


def _skip_commands_for_type(clean_type):
    if clean_type == OutputCleaner.DeviceType.HUAWEI:
        return {"screen-length 0 temporary"}
    if clean_type == OutputCleaner.DeviceType.H3C:
        return {"screen-length disable"}
    if clean_type == OutputCleaner.DeviceType.RGOS:
        return {"terminal length 0", "terminal width 0"}
    return set()


def _command_timeout(command):
    lower = command.lower()
    if "display current-configuration" in lower:
        return 180.0
    if "show full-configuration" in lower:
        return 180.0
    if "show running-config" in lower:
        return 180.0
    return 20.0


def _format_outputs(outputs, clean_type, prompt):
    if clean_type == OutputCleaner.DeviceType.HUAWEI:
        order = [
            "display version",
            "display device",
            "display current-configuration",
        ]
    elif clean_type == OutputCleaner.DeviceType.H3C:
        order = [
            "display version",
            "display device",
            "display device manuinfo",
            "display current-configuration",
        ]
    else:
        order = [
            "get system status | grep .",
            "get system ha status | grep .",
            "get hardware status | grep .",
            "show full-configuration | grep .",
        ]
    if clean_type == OutputCleaner.DeviceType.RGOS:
        order = [
            "show version",
            "show running-config",
        ]
    parts = []
    for cmd in order:
        if cmd in outputs:
            parts.append(_format_block(cmd, outputs[cmd], prompt))
    return "\n\n".join(parts)


def _format_block(cmd, output, prompt):
    header = f"##### {cmd} #####"
    cleaned = _clean_command_output(output, cmd, prompt)
    return f"{header}\n{cleaned}".rstrip()


def _clean_command_output(output, cmd, prompt):
    lines = output.split("\n")
    cleaned = []
    for line in lines:
        stripped = line.strip()
        if not stripped:
            continue
        if cmd in line:
            continue
        if re.match(r"^<[^>]+>$", stripped):
            continue
        if re.match(r"^\\[[^\\]]+\\]$", stripped):
            continue
        if prompt and prompt in line:
            continue
        cleaned.append(line.rstrip())
    return "\n".join(cleaned)


def _detect_paging(output):
    if not output:
        return False
    lower = output.lower()
    return ("--more--" in lower) or ("---- more ----" in lower) or ("more" in lower and "press" in lower)
