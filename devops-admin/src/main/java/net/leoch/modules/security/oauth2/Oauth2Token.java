

package net.leoch.modules.security.oauth2;

import org.apache.shiro.authc.AuthenticationToken;

/**
 * token
 *
 * @author Taohongqiang
 */
public class Oauth2Token implements AuthenticationToken {
    private String token;

    public Oauth2Token(String token){
        this.token = token;
    }

    @Override
    public String getPrincipal() {
        return token;
    }

    @Override
    public Object getCredentials() {
        return token;
    }
}
