package com.whatsys.client;

import com.whatsys.shared.FieldVerifier;
import com.google.gwt.core.client.JsArray;
import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyUpEvent;
import com.google.gwt.event.dom.client.KeyUpHandler;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.DialogBox;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.whatsys.client.StMessage;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;

public class Play implements EntryPoint {

    private static final String SERVER_ERROR = "D'oh!";

    private static final String JSON_URL = "http://127.0.0.1:8888/ststream.json";

    private final GreetingServiceAsync greetingService = GWT.create(GreetingService.class);
    private final native StStreamResponse streamResponse(String json) /*-{return eval(json);}-*/;
    private final native JsArray<StMessage> getMessages(String json) /*-{return eval("(" +json + ")").messages;}-*/;

    public void onModuleLoad() {
        final Button sendButton = new Button("Send");
        final TextBox nameField = new TextBox();
        final Label errorLabel = new Label();
        nameField.setText("GWT User");
        sendButton.addStyleName("sendButton");
        RootPanel.get("nameFieldContainer").add(nameField);
        RootPanel.get("sendButtonContainer").add(sendButton);
        RootPanel.get("errorLabelContainer").add(errorLabel);
        nameField.setFocus(true);
        nameField.selectAll();
        final DialogBox dialogBox = new DialogBox();
        dialogBox.setText("RPC");
        dialogBox.setAnimationEnabled(true);
        final Button closeButton = new Button();
        closeButton.setText("Fuck this");
        closeButton.getElement().setId("closeButton");
        final Label textToServerLabel = new Label();
        final HTML serverResponseLabel = new HTML();
        VerticalPanel dialogVPanel = new VerticalPanel();
        {
            dialogVPanel.setHorizontalAlignment(VerticalPanel.ALIGN_RIGHT);
            dialogVPanel.addStyleName("dialogVPanel");
            dialogVPanel.add(new HTML("<b>My super dumb html blob</b>"));
            dialogVPanel.add(textToServerLabel);
            dialogVPanel.add(new HTML("<br><b> Server replies: </b>"));
            dialogVPanel.add(serverResponseLabel);
            dialogVPanel.add(closeButton);
        }
        dialogBox.setWidget(dialogVPanel);
        closeButton.addClickHandler(new ClickHandler() {

            public void onClick(ClickEvent e) {
                dialogBox.hide();
                sendButton.setEnabled(true);
                sendButton.setFocus(true);
            }
        });
        class MyHandler implements ClickHandler, KeyUpHandler {

            public void onClick(ClickEvent e) {
                this.sendNameToServer();
                this.doSomeCrazyShit();
                GWT.log("My balls are on fire");
            }

            public void onKeyUp(KeyUpEvent e) {
                if (e.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
                    this.sendNameToServer();
                }
            }

            public void doSomeCrazyShit() {
                RequestBuilder builder = new RequestBuilder(RequestBuilder.GET, JSON_URL);
                try {
                    Request request = builder.sendRequest(null, new RequestCallback() {

                        public void onError(Request request, Throwable e) {
                        }

                        public void onResponseReceived(Request request, Response response) {
                            if (200 == response.getStatusCode()) {
                                String responseText = response.getText();
                                GWT.log(responseText);
                                JsArray<StMessage> calamity = getMessages(responseText);
                                GWT.log(calamity.get(0).jsbody());
                            } else {
                                GWT.log("MEGAFAIL");
                            }
                        }
                    });
                } catch (RequestException e) {
                    GWT.log("MEGAFAIL2");
                } finally {
                }
            }

            public void sendNameToServer() {
                errorLabel.setText("");
                String textToServer = nameField.getText();
                if (!FieldVerifier.isValidName(textToServer)) {
                    errorLabel.setText("I'm afraid I can't let you do that, Bingus...");
                    return;
                }
                textToServerLabel.setText(textToServer);
                serverResponseLabel.setText("");
                greetingService.greetServer(textToServer, new AsyncCallback<String>() {

                    public void onSuccess(String result) {
                        dialogBox.setText("We have received a message. It would appear that it is from the server, m'lud.");
                        serverResponseLabel.removeStyleName("serverResponseLabelError");
                        {
                            serverResponseLabel.setHTML(result);
                            dialogBox.center();
                            closeButton.setFocus(true);
                        }
                    }

                    public void onFailure(java.lang.Throwable e) {
                        dialogBox.setText("something is wrong :(");
                        GWT.log(e.toString());
                        serverResponseLabel.addStyleName("serverResponseLabelError");
                        {
                            serverResponseLabel.setHTML(SERVER_ERROR);
                            dialogBox.center();
                            closeButton.setFocus(true);
                        }
                    }
                });
            }
        }
        MyHandler handler = new MyHandler();
        sendButton.addClickHandler(handler);
        nameField.addKeyUpHandler(handler);
    }
}
