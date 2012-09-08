package com.whatsys.client;

import com.google.gwt.core.client.JavaScriptObject;

class StockData extends JavaScriptObject {

    protected StockData() {
    }
    public final native String jssymbol() /*-{return this.symbol;}-*/;
    public final native double jsprice() /*-{return this.price;}-*/;
    public final native double jschange() /*-{return this.change;}-*/;

    public final double getChangeRatio() {
        return this.jschange() / this.jsprice();
    }
}
