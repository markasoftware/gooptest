#define CTRL_PIN A0

#define PWM_PIN 11

int old_analog;

void setup() {
	old_analog = 0;
	pinMode(PWM_PIN, OUTPUT);
}

void loop() {
	int new_analog = analogRead(CTRL_PIN);
	if (abs(new_analog - old_analog) > 3) {
		analogWrite(PWM_PIN, new_analog >> 2);
		old_analog = new_analog;
	}
}
