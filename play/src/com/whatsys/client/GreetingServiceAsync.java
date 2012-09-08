package com.whatsys.client;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface GreetingServiceAsync {

    void greetServer(String name, AsyncCallback<String> callback) throws IllegalArgumentException;
}
