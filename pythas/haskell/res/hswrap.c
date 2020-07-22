#include <HsFFI.h>

static void start_hsrts(void) __attribute__((constructor));
static void start_hsrts(void) {
  static char *argv[] = { 0 }, **argv_ = argv;
  static int argc = 0;
  hs_init(&argc, &argv_);
}

static void stop_hsrts(void) __attribute__((destructor));
static void stop_hsrts(void) {
  hs_exit();
}