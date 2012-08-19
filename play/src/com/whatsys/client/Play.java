package com.whatsys.client;

import com.whatsys.shared.FieldVerifier;
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

public class Play implements EntryPoint {

    private static final String SERVER_ERROR = "D'oh!";

    private final GreetingServiceAsync greetingService = GWT.create(GreetingService.class);

    public void onModuleLoad() {
        final Button sendButton = new Button("Send");
        final TextBox nameField = new TextBox();
        nameField.setText("GWT User");
        final Label errorLabel = new Label();
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
        closeButton.getElement().setId("closeButton");
        final Label textToServerLabel = new Label();
        final HTML serverResponseLabel = new HTML();
        VerticalPanel dialogVPanel = new VerticalPanel();
        dialogVPanel.addStyleName("dialogVPanel");
        dialogVPanel.add(new HTML("<b> Sending name to the server: </b>"));
        dialogVPanel.add(textToServerLabel);
        dialogVPanel.add(new HTML("<br><b> Server replies: </b>"));
        dialogVPanel.add(serverResponseLabel);
        dialogVPanel.setHorizontalAlignment(VerticalPanel.ALIGN_RIGHT);
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
            }

            public void onKeyUp(KeyUpEvent e) {
                if (e.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
                    this.sendNameToServer();
                }
            }

            public void sendNameToServer() {
                errorLabel.setText("");
                String textToServer = nameField.getText();
                if (!FieldVerifier.isValidName(textToServer)) {
                    errorLabel.setText("Please do the thing right");
                    return;
                }
                textToServerLabel.setText(textToServer);
                serverResponseLabel.setText("");
                greetingService.greetServer(textToServer, new AsyncCallback<String>() {

                    public void onSuccess(String result) {
                        dialogBox.setText("RPC Success Sauce (hax)");
                        serverResponseLabel.removeStyleName("serverResponseLabelError");
                        {
                            serverResponseLabel.setHTML(result);
                            dialogBox.center();
                            closeButton.setFocus(true);
                        }
                    }

                    public void onFailure(java.lang.Throwable e) {
                        dialogBox.setText("megaFAIL");
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
