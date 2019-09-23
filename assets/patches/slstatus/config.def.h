const unsigned int interval = 1000;
static const char unknown_str[] = "n/a";
#define MAXLEN 2048
static const struct arg args[] = {
  { cpu_perc, "C:%3s%% | ", NULL },
  { ram_perc, "R:%3s%% | ", NULL },
  { wifi_perc, "W:%3s%% | ", "wlp1s0" },
  { vol_perc, "V:%3s%% | ", "/dev/mixer" },
  { battery_perc, "B:%3s%% | ", "BAT" },
	{ datetime, "%s", "%F %T" },
};
