void setup() {
	// serial send baud rate just controls down how fast it sends -- error (eg,
	// divisibility with the clock rate) doesn't matter when simulating.
	Serial.begin(115200);
}

void loop() {
	String str = Serial.readStringUntil('\n');
	if (str == "wait") {
		delay(250);
	} else {
		for (unsigned char i = 0; i < str.length(); i++) {
			Serial.write(i % 2 ? toupper(str[i]) : tolower(str[i]));
		}
		Serial.flush();
	}
}
