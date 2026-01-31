import logging
import re


class OutputCleaner:
    class DeviceType:
        HUAWEI = "HUAWEI"
        H3C = "H3C"
        FORTI = "FORTI"
        RGOS = "RGOS"
        GENERIC = "GENERIC"

    @staticmethod
    def clean_output(raw, device_type):
        if raw is None or not str(raw).strip():
            logging.error("raw output is empty")
            return raw
        try:
            if device_type == OutputCleaner.DeviceType.HUAWEI:
                return _clean_huawei_output(raw)
            if device_type == OutputCleaner.DeviceType.H3C:
                return _clean_h3c_output(raw)
            if device_type == OutputCleaner.DeviceType.FORTI:
                return _clean_forti_output(raw)
            return raw
        except Exception as e:
            logging.warning("failed to clean output: %s", e)
            return raw


def _clean_huawei_output(text):
    commands = [
        "screen-length 0 temporary",
        "display version",
        "display device",
        "display current-configuration",
    ]
    blocks = _extract_blocks_by_commands(text, commands, prompt_hint="<")
    dv = blocks.get("display version", "")
    dd = blocks.get("display device", "")
    dc = blocks.get("display current-configuration", "")
    return _join_blocks([
        _format_block("display version", dv),
        _format_block("display device", dd),
        _format_block("display current-configuration", dc),
    ])


def _clean_h3c_output(text):
    commands = [
        "screen-length disable",
        "display version",
        "display device",
        "display device manuinfo",
        "display current-configuration",
    ]
    blocks = _extract_blocks_by_commands(text, commands, prompt_hint="<")
    dv = blocks.get("display version", "")
    dd = blocks.get("display device", "")
    ddm = blocks.get("display device manuinfo", "")
    dc = blocks.get("display current-configuration", "")
    return _join_blocks([
        _format_block("display version", dv),
        _format_block("display device", dd),
        _format_block("display device manuinfo", ddm),
        _format_block("display current-configuration", dc),
    ])


def _clean_forti_output(text):
    commands = [
        "get system status | grep .",
        "get system ha status | grep .",
        "get hardware status | grep .",
        "show full-configuration | grep .",
    ]
    blocks = _extract_blocks_by_commands(text, commands, prompt_hint=None)
    ss = blocks.get("get system status | grep .", "")
    shs = blocks.get("get system ha status | grep .", "")
    hs = blocks.get("get hardware status | grep .", "")
    sf = blocks.get("show full-configuration | grep .", "")
    return _join_blocks([
        _format_block("get system status | grep .", ss),
        _format_block("get system ha status | grep .", shs),
        _format_block("get hardware status | grep .", hs),
        _format_block("show full-configuration | grep .", sf),
    ])


def _clean_forti_full_config(text):
    try:
        lines = text.split("\n")
        extracted = []
        capturing = False
        for line in lines:
            if "-fwl-" in line and line.strip().endswith("show full-configuration | grep ."):
                capturing = True
                continue
            if capturing and "-fwl-" in line and line.strip().endswith("# quit"):
                capturing = False
                break
            if capturing:
                extracted.append(line)
        if extracted:
            extracted.pop(0)
        i = len(extracted) - 1
        while i >= 0:
            if "end" in extracted[i]:
                break
            extracted.pop(i)
            i -= 1
        return "\n".join(extracted)
    except Exception:
        return None


def _extract_with_regex(text, regex):
    run = "grep ." not in regex
    pattern = re.compile(regex, re.DOTALL)
    m = pattern.search(text or "")
    if not m:
        return None
    try:
        content = m.group(1)
        if "return" in regex:
            content = content + "return"
        if run:
            content = _remove_uptime_line(content)
        return content.strip()
    except IndexError:
        return None


def _extract_command_block(text, command, stop_on_return=False):
    if not text:
        return ""
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    block = _extract_with_prompt_regex(text, command)
    if not block:
        block = _extract_with_scan(text, command, stop_on_return)
    if stop_on_return and "return" in block:
        idx = block.rfind("return")
        block = block[: idx + len("return")]
    block = _remove_uptime_line(block)
    block = _filter_lines(block)
    return block.strip() if block else ""


def _extract_with_prompt_regex(text, command):
    cmd = re.escape(command)
    pattern = re.compile(
        rf"{cmd}\\s*\\r?\\n([\\s\\S]*?)(?=\\n\\s*[^\\s].*[>#\\]]\\s*$)",
        re.MULTILINE,
    )
    m = pattern.search(text)
    if not m:
        return ""
    return m.group(1)


def _extract_with_scan(text, command, stop_on_return):
    lines = text.splitlines()
    start = None
    for i, line in enumerate(lines):
        if command in line:
            start = i + 1
            break
    if start is None:
        return ""
    collected = []
    for i in range(start, len(lines)):
        line = lines[i]
        if stop_on_return and "return" in line:
            collected.append(line)
            break
        if _is_prompt_line(line):
            break
        collected.append(line)
    return "\n".join(collected)


def _remove_uptime_line(content):
    if content is None:
        return None
    lines = content.split("\n")
    result = []
    for line in lines:
        if "uptime" not in line.lower():
            result.append(line)
    return "\n".join(result)


def _filter_lines(content):
    if content is None:
        return None
    lines = content.split("\n")
    filtered = []
    command_exact = {
        "screen-length 0 temporary",
        "screen-length disable",
        "display version",
        "display device",
        "display device manuinfo",
        "display current-configuration",
        "get system status | grep .",
        "get system ha status | grep .",
        "get hardware status | grep .",
        "show full-configuration | grep .",
    }
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("#******************"):
            filtered.append(line)
            continue
        if not stripped:
            filtered.append(line)
            continue
        if stripped.lower() in command_exact:
            continue
        if re.match(r"^<[^>]+>.*$", stripped):
            continue
        if re.match(r"^.+\\s#\\s+.+$", stripped):
            # forti prompt + command
            continue
        if re.match(r"^.+#\\s*$", stripped) or stripped.endswith("#"):
            # forti prompt only
            continue
        if _is_prompt_line(line):
            continue
        if stripped == "return":
            continue
        filtered.append(line)
    return "\n".join(filtered)


def _join_blocks(blocks):
    cleaned = []
    for block in blocks:
        cleaned.append(block.strip())
    return "\n\n".join(cleaned)


def _format_block(command, content):
    header = f"#******************{command}******************"
    body = content.strip() if content else ""
    return f"{header}\n{body}".rstrip()


def _extract_forti_block_regex(text, command):
    if not text:
        return ""
    normalized = text.replace("\r\n", "\n").replace("\r", "\n")
    cmd = re.escape(command)
    pattern = re.compile(
        rf"^.*#\\s*{cmd}\\s*$\\n([\\s\\S]*?)(?=\\n.*#\\s*(?:$|\\w))",
        re.MULTILINE,
    )
    m = pattern.search(normalized)
    if not m:
        return ""
    return _filter_lines(m.group(1))


def _extract_blocks_by_commands(text, commands, prompt_hint=None):
    if not text:
        return {}
    normalized = text.replace("\r\n", "\n").replace("\r", "\n")
    lines = normalized.splitlines()
    markers = []
    for i, line in enumerate(lines):
        for cmd in commands:
            if _is_command_line(line, cmd, prompt_hint):
                markers.append((i, cmd))
                break
    if not markers:
        return {}
    blocks = {}
    for idx, (start, cmd) in enumerate(markers):
        end = len(lines)
        if idx + 1 < len(markers):
            end = markers[idx + 1][0]
        chunk = "\n".join(lines[start + 1:end])
        chunk = _filter_lines(chunk)
        blocks[cmd] = chunk
    return blocks


def _is_command_line(line, command, prompt_hint=None):
    if line.strip() == command:
        return True
    if command not in line:
        return False
    stripped = line.strip()
    if prompt_hint and prompt_hint not in stripped:
        return False
    if stripped.startswith("<"):
        return True
    if ">" in stripped or "#" in stripped:
        return True
    return False


def _extract_blocks_by_exact_lines(text, commands):
    if not text:
        return {}
    normalized = text.replace("\r\n", "\n").replace("\r", "\n")
    lines = normalized.splitlines()
    markers = []
    for i, line in enumerate(lines):
        stripped = line.strip()
        for cmd in commands:
            if stripped == cmd:
                markers.append((i, cmd))
                break
    if not markers:
        return {}
    blocks = {}
    for idx, (start, cmd) in enumerate(markers):
        end = len(lines)
        if idx + 1 < len(markers):
            end = markers[idx + 1][0]
        chunk = "\n".join(lines[start + 1:end])
        blocks[cmd] = _filter_lines(chunk)
    return blocks


def _extract_forti_block(text, command):
    if not text:
        return ""
    normalized = text.replace("\r\n", "\n").replace("\r", "\n")
    cmd = re.escape(command)
    pattern = re.compile(rf"^.+#\\s*{cmd}\\s*$", re.MULTILINE)
    m = pattern.search(normalized)
    if not m:
        # fallback: find command anywhere on line
        pattern = re.compile(rf"^{cmd}\\s*$", re.MULTILINE)
        m = pattern.search(normalized)
    if not m:
        return ""
    start = m.end()
    tail = normalized[start:].lstrip("\n")
    lines = tail.splitlines()
    collected = []
    for line in lines:
        if re.match(r"^.+\\s#\\s*$", line.strip()):
            break
        if re.match(r"^.+\\s#\\s+.+$", line.strip()):
            break
        collected.append(line)
    return _filter_lines("\n".join(collected))


def _is_prompt_line(line):
    if not line:
        return False
    stripped = line.strip()
    if not stripped:
        return False
    if not re.match(r"^[<\\[]?[^\\s]+[>#\\]]$", stripped):
        return False
    lower = stripped.lower()
    for token in ("display", "show", "get system", "screen-length", "quit", "exit"):
        if token in lower:
            return False
    return True
