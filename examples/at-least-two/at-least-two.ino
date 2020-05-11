#define LED_PIN 13

#define BUTTON_1_PIN 4
#define BUTTON_2_PIN 5
#define BUTTON_3_PIN 6

void setup() {
	pinMode(LED_PIN, OUTPUT);
}

void loop() {
	char k = 0;
	if (digitalRead(BUTTON_1_PIN)) k++;
	if (digitalRead(BUTTON_2_PIN)) k++;
	if (digitalRead(BUTTON_3_PIN)) k++;
	digitalWrite(LED_PIN, k >= 2 ? HIGH : LOW);
}
