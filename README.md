# scala-aprs
A library for processing the aprs-is or firenet feed on spark, in order to forecast local Hazard conditions and send warnings.

Specification document for the input format is at http://www.aprs.org/doc/APRS101.PDF, with relevant addenda at http://www.aprs.org/aprs12.html and http://aprs.org/aprs11/spec-wx.txt.

Achitectural goal:

Initial input one or more of:
APRS/Firenet data feed
Stored data feed in the above APRS format.
A list of locations and "Alert" rules for each location.

Using Spark Streaming, the input APRS formatted observations will be converted to an analysis and storage ready format, the "analytic dataset".

The Analytic dataset will then be used as an input to "Model" subclasses, all of which read the inputs, and create a near-term model for a particular type of value at a particular location that will allow at least for:
 - The querying of values at a particular time, not necessarily the same as the original observation times.
 - The querying of a maximum expected over a time interval (possibly fixed to a day or half day)
 - The querying of a minimum expected over a time interval (possibly fixed to a day or half day)

These Models will be created on demand for Alert objects that implement the specified alerts, and query the models to determine if their particular alert should be fired. When an alert is fired, it shall write a line to a file indicating the alert that fired and give a message describing the alert state.

Eventually, instead of this (or in addition to) this file, an email or HTTP action can be taken (emailing a text message to a phone, or firing an ITTT action).
