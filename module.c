#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <linux/input.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

#include <emacs-module.h>

int plugin_is_GPL_compatible;

// Symbols
static emacs_value Qnil;
static emacs_value Qt;

// Global event structure
static struct input_event ev;

// fin func
static void
fin_close(void *fdptr)
{
  int fd = (intptr_t)fdptr;
  if (fd != -1)
    close(fd);
}

static emacs_value
FopenDevice(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
  char buf[64];

  (void)ptr;
  (void)n;
  int id = env->extract_integer(env, args[0]);
  int buflen = sprintf(buf, "/dev/input/event%d", id);
  int fd = open(buf, O_RDONLY);

  if (fd < 0) {
    emacs_value signal = env->intern(env, "file-error");
    emacs_value message = env->make_string(env, buf, buflen);
    env->non_local_exit_signal(env, signal, message);
    return Qnil;
  }

  return env->make_user_ptr(env, fin_close, (void*)(intptr_t)fd);
}

static emacs_value
FcloseDevice(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
  (void)ptr;
  (void)n;

  int fd = (intptr_t)env->get_user_ptr(env, args[0]);
  int rc = close(fd);

  if (rc < 0) {
    return Qnil;
  } else {
    return Qt;
  }
}

static emacs_value
FreadEvent(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
  (void)ptr;
  (void)n;

  int fd = (intptr_t)env->get_user_ptr(env, args[0]);
  int rc = read(fd, &ev, sizeof(struct input_event));

  if (rc < 0) {
    /* emacs_value signal = env->intern(env, "file-error"); */
    /* emacs_value message = env->make_string(env, buf, buflen); */
    /* env->non_local_exit_signal(env, signal, message); */
    return Qnil;
  }

  emacs_value vec = args[1];
  env->vec_set(env, vec, 0, env->make_integer(env, ev.time.tv_usec));
  env->vec_set(env, vec, 1, env->make_integer(env, ev.code));
  env->vec_set(env, vec, 2, env->make_integer(env, ev.type));
  env->vec_set(env, vec, 3, env->make_integer(env, ev.value));
  return vec;
};

static emacs_value
FgrabDevice(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
  (void)ptr;
  (void)n;

  int fd = (intptr_t)env->get_user_ptr(env, args[0]);
  int rc = ioctl(fd, EVIOCGRAB, 1);

  if (rc < 0) {
    return Qnil;
  } else {
    return Qt;
  }
}

static emacs_value
FungrabDevice(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr) {
  (void)ptr;
  (void)n;

  int fd = (intptr_t)env->get_user_ptr(env, args[0]);
  int rc = ioctl(fd, EVIOCGRAB, 1);

  if (rc < 0) {
    return Qnil;
  } else {
    return Qt;
  }
}

static emacs_value
FmymodTest (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  return env->make_integer (env, 42);
}

static void
bindFunction (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);

  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

static void
provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  // env
  emacs_env *env = ert->get_environment (ert);

  // gather symbols
  Qnil = env->intern(env, "nil");
  Qt   = env->intern(env, "t");

  // bind functions
  bindFunction(env,
               "kg-test",
               env->make_function(env, 0, 0, FmymodTest, "doc", NULL));
  bindFunction(env,
               "kg-open-device",
               env->make_function(env, 1, 1, FopenDevice, "doc", NULL));
  bindFunction(env,
               "kg-close-device",
               env->make_function(env, 1, 1, FcloseDevice, "doc", NULL));
  bindFunction(env,
               "kg-read-event",
               env->make_function(env, 2, 2, FreadEvent, "doc", NULL));
  bindFunction(env,
               "kg-grab-device",
               env->make_function(env, 1, 1, FgrabDevice, "doc", NULL));
  bindFunction(env,
               "kg-ungrab-device",
               env->make_function(env, 1, 1, FungrabDevice, "doc", NULL));

  // provide feature
  provide (env, "keyboard-grabber");

    // loaded successfully
    return 0;
  }
