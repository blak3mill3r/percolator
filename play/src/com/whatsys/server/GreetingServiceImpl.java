package com.whatsys.server;

import com.whatsys.client.GreetingService;
import com.whatsys.shared.FieldVerifier;
import com.google.gwt.user.server.rpc.RemoteServiceServlet;

public class GreetingServiceImpl extends RemoteServiceServlet implements GreetingService {

    public String greetServer(String input) throws IllegalArgumentException {
        if (!FieldVerifier.isValidName(input)) {
            throw new IllegalArgumentException("Not cool with the server, sorry");
        } else {
            return "The server's nuts are on fire ooh!";
        }
    }
}
