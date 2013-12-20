package org.cyclop.service.converter;

import org.cyclop.model.UserPreferences;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * @author Maciej Miklas
 */
public class TestJsonMarshaller {

    private JsonMarshaller marshaller = new JsonMarshaller();

    @Test
    public void testMarshal() {
        UserPreferences up = new UserPreferences();
        up.setShowCqlCompletionHint(false);
        String res = marshaller.marshal(up);
        assertEquals("{\"e_hi\":\"0\",\"e_he\":\"1\"}", res);
    }

    @Test
    public void testUnmarshal() {
        UserPreferences res = marshaller.unmarshal(UserPreferences.class, "{\"e_hi\":\"0\",\"e_he\":\"1\"}");

        UserPreferences up = new UserPreferences();
        up.setShowCqlCompletionHint(false);
        assertEquals(up, res);
    }

    @Test
    public void testUnmarshalDefaults() {
        UserPreferences res = marshaller.unmarshal(UserPreferences.class, "{\"e_hi\":\"0\"}");

        UserPreferences up = new UserPreferences();
        up.setShowCqlCompletionHint(false);
        assertEquals(up, res);
    }

}
