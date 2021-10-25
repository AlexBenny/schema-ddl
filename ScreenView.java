
package com.snowplowanalytics.snowplow.event;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.HashMap;
import java.util.Map;

public class ScreenView extends AbstractSelfDescribing {

    public final static String SCHEMA_SCREENVIEW = "iglu:com.snowplowanalytics.mobile/screen_view/jsonschema/1-0-0";

    
    public final static String PARAM_ID = "id";
    
    public final static String PARAM_NAME = "name";
    
    public final static String PARAM_PREVIOUSID = "previousId";
    
    public final static String PARAM_PREVIOUSNAME = "previousName";
    
    public final static String PARAM_PREVIOUSTYPE = "previousType";
    
    public final static String PARAM_TRANSITIONTYPE = "transitionType";
    
    public final static String PARAM_TYPE = "type";
    

    
    /// It's the property for `id` JSON key
    public String id;
    
    /// It's the property for `name` JSON key
    public String name;
    
    /// It's the property for `previousId` JSON key
    public String previousId;
    
    /// It's the property for `previousName` JSON key
    public String previousName;
    
    /// It's the property for `previousType` JSON key
    public String previousType;
    
    /// It's the property for `transitionType` JSON key
    public String transitionType;
    
    /// It's the property for `type` JSON key
    public String type;
    

    public ScreenView(id: String, name: String, previousId: String, previousName: String, previousType: String, transitionType: String, type: String) {
        
        this.id = id;
        
        this.name = name;
        
        this.previousId = previousId;
        
        this.previousName = previousName;
        
        this.previousType = previousType;
        
        this.transitionType = transitionType;
        
        this.type = type;
        
        // Set here further checks about the arguments.
    }

    // Builder methods
    
    @NonNull
    public ScreenView id ( String id) {
        this.id = id;
        return this;
    }
    
    @NonNull
    public ScreenView name ( String name) {
        this.name = name;
        return this;
    }
    
    @NonNull
    public ScreenView previousId ( String previousId) {
        this.previousId = previousId;
        return this;
    }
    
    @NonNull
    public ScreenView previousName ( String previousName) {
        this.previousName = previousName;
        return this;
    }
    
    @NonNull
    public ScreenView previousType ( String previousType) {
        this.previousType = previousType;
        return this;
    }
    
    @NonNull
    public ScreenView transitionType ( String transitionType) {
        this.transitionType = transitionType;
        return this;
    }
    
    @NonNull
    public ScreenView type ( String type) {
        this.type = type;
        return this;
    }
    

    // Tracker methods

    @Override
    public @NonNull Map<String, Object> getDataPayload() {
        HashMap<String,Object> payload = new HashMap<>();
        
        if (id != null) {
            payload.put(PARAM_ID, id);
        }
        
        if (name != null) {
            payload.put(PARAM_NAME, name);
        }
        
        if (previousId != null) {
            payload.put(PARAM_PREVIOUSID, previousId);
        }
        
        if (previousName != null) {
            payload.put(PARAM_PREVIOUSNAME, previousName);
        }
        
        if (previousType != null) {
            payload.put(PARAM_PREVIOUSTYPE, previousType);
        }
        
        if (transitionType != null) {
            payload.put(PARAM_TRANSITIONTYPE, transitionType);
        }
        
        if (type != null) {
            payload.put(PARAM_TYPE, type);
        }
        
        return payload;
    }

    @Override
    public @NonNull String getSchema() {
        return SCHEMA_SCREENVIEW;
    }
}
