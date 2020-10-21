package microtope.messages;

import javax.jms.Destination;
import javax.jms.JMSException;
import java.util.Enumeration;

/** Mostly Garbage, I've drawn a line from where its important
 *
 */
public class MockTextMessage implements javax.jms.TextMessage{

    public String text;
    public boolean throwsJMSException = false;

    @Override
    public void setText(String s) throws JMSException {
        if(throwsJMSException)
            throw new JMSException("Bad");
        else
            text = s;
    }

    @Override
    public String getText() throws JMSException {
        if(throwsJMSException)
            throw new JMSException("Bad");
        else
            return text;
    }
    /*
    ======================================================================
                        Dummies from here
    ======================================================================
     */
    public String getJMSMessageID() throws JMSException {return null;}
    public void setJMSMessageID(String s) throws JMSException {}
    public long getJMSTimestamp() throws JMSException {return 0;}
    public void setJMSTimestamp(long l) throws JMSException {}
    public byte[] getJMSCorrelationIDAsBytes() throws JMSException {return new byte[0];}
    public void setJMSCorrelationIDAsBytes(byte[] bytes) throws JMSException {}
    public void setJMSCorrelationID(String s) throws JMSException {}
    public String getJMSCorrelationID() throws JMSException {return null;}
    public Destination getJMSReplyTo() throws JMSException {return null;}
    public void setJMSReplyTo(Destination destination) throws JMSException {}
    public Destination getJMSDestination() throws JMSException {return null;}
    public void setJMSDestination(Destination destination) throws JMSException {}
    public int getJMSDeliveryMode() throws JMSException {return 0;}
    public void setJMSDeliveryMode(int i) throws JMSException {}
    public boolean getJMSRedelivered() throws JMSException {return false;}
    public void setJMSRedelivered(boolean b) throws JMSException {}
    public String getJMSType() throws JMSException {return null;}
    public void setJMSType(String s) throws JMSException {}
    public long getJMSExpiration() throws JMSException {return 0;}
    public void setJMSExpiration(long l) throws JMSException {}
    public int getJMSPriority() throws JMSException {return 0;}
    public void setJMSPriority(int i) throws JMSException {}
    public void clearProperties() throws JMSException {}
    public boolean propertyExists(String s) throws JMSException {return false;}
    public boolean getBooleanProperty(String s) throws JMSException {return false;}
    public byte getByteProperty(String s) throws JMSException {return 0;}
    public short getShortProperty(String s) throws JMSException {return 0;}
    public int getIntProperty(String s) throws JMSException {return 0;}
    public long getLongProperty(String s) throws JMSException {return 0;}
    public float getFloatProperty(String s) throws JMSException {return 0;}
    public double getDoubleProperty(String s) throws JMSException {return 0;}
    public String getStringProperty(String s) throws JMSException {return null;}
    public Object getObjectProperty(String s) throws JMSException {return null;}
    public Enumeration getPropertyNames() throws JMSException {return null;}
    public void setBooleanProperty(String s, boolean b) throws JMSException {}
    public void setByteProperty(String s, byte b) throws JMSException {}
    public void setShortProperty(String s, short i) throws JMSException {}
    public void setIntProperty(String s, int i) throws JMSException {}
    public void setLongProperty(String s, long l) throws JMSException {}
    public void setFloatProperty(String s, float v) throws JMSException {}
    public void setDoubleProperty(String s, double v) throws JMSException {}
    public void setStringProperty(String s, String s1) throws JMSException {}
    public void setObjectProperty(String s, Object o) throws JMSException {}
    public void acknowledge() throws JMSException {}
    public void clearBody() throws JMSException {}
}
