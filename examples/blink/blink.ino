#define BLINK_PIN 13
#define BLINK_DELAY 500

void setup() {
	pinMode(BLINK_PIN, OUTPUT);
}

void loop() {
	digitalWrite(BLINK_PIN, LOW);
	delay(BLINK_DELAY);
	digitalWrite(BLINK_PIN, HIGH);
	delay(BLINK_DELAY);
}
