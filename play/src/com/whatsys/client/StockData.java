package com.whatsys.client;

import com.google.gwt.core.client.JavaScriptObject;

class StockData extends JavaScriptObject {

    protected StockData() {
    }
    private final native String jssymbol() /*-{return this.symbol;}-*/;
    private final native double jsprice() /*-{return this.price;}-*/;
    private final native double jschange() /*-{return this.change;}-*/;

    public final double getChangeRatio() {
        return this.jschange() / this.jsprice();
    }
}
