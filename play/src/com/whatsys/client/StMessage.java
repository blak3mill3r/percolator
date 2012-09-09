package com.whatsys.client;

import com.google.gwt.core.client.JavaScriptObject;

class StMessage extends JavaScriptObject {

    protected StMessage() {
    }
    public final native int jsid() /*-{return this.id;}-*/;
    public final native String jsbody() /*-{return this.body;}-*/;
    public final native String jscreated_at() /*-{return this.created_at;}-*/;
}
