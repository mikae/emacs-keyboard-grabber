#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <linux/input.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

#include <libevdev-1.0/libevdev/libevdev.h>

static volatile int closed = 0;

void intHandler(int dummy) {
  closed = 1;
  printf("Closed\n");
}

int main (int argc, char **argv) {
  const char* devicePath;

  signal(SIGINT, intHandler);

  if (argc > 1) {
    devicePath = argv[1];
  } else {
    printf("Device path wasn't provided\n");
    exit(1);
  }

  struct libevdev *dev = NULL;
  int fd;
  int rc = 1;
  fd = open(devicePath, O_RDONLY);
  rc = libevdev_new_from_fd(fd, &dev);

  if (rc < 0) {
    fprintf(stderr, "Failed to init libevdev: (%s)", strerror(-rc));
    exit(1);
  }

  printf("Input device name: \"%s\"\n", libevdev_get_name(dev));
  printf("Input device ID: bus %#x vendor %#x product %#x\n",
         libevdev_get_id_bustype(dev),
         libevdev_get_id_vendor(dev),
         libevdev_get_id_product(dev));

  libevdev_grab(dev, LIBEVDEV_GRAB);

  do {
    struct input_event ev;
    rc = libevdev_next_event(dev, LIBEVDEV_READ_FLAG_NORMAL, &ev);
    if (rc == 0)
      printf("Event: %d %d %d\n",
             ev.type,
             ev.code,
             ev.value);
  } while ((rc == 1 || rc == 0 || rc == -EAGAIN) && !closed);

  libevdev_grab(dev, LIBEVDEV_UNGRAB);
  libevdev_free(dev);
}
