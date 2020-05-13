#define CTRL_PIN A0

int old_analog;

void setup() {
	old_analog = 0;
	Serial.begin(38400);
	// just to test things out
	delay(50);
	Serial.print("Hello World!");
	Serial.flush();
}

void loop() {
	int new_analog = analogRead(CTRL_PIN);
	if (abs(new_analog - old_analog) > 3) {
		old_analog = new_analog;
		// make the 10-bit data fit in a byte
		Serial.write(new_analog >> 2);
		Serial.flush();
	}
}
