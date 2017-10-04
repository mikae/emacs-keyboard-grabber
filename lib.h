#ifndef LIB_H
#define LIB_H
#include <xcb/xproto.h>

struct atom_cache_entry {
    xcb_atom_t atom;
    const char *name;
    xcb_intern_atom_cookie_t intern_atom;
    struct atom_cache_entry *next;
};

struct atom_cache_entry *intern_atom (xcb_connection_t *dpy,
                                      const char *name);

xcb_atom_t get_atom(xcb_connection_t *dpy,
                    const char *name);


void* get_window_property(xcb_connection_t *dpy,
                          xcb_window_t window,
                          xcb_atom_t property,
                          xcb_atom_t type,
                          uint32_t length);

pid_t get_window_pid(xcb_connection_t *dpy,
                     xcb_window_t window);

void setup_display_and_screen(const char *display_name,
                              xcb_connection_t **dpy,
                              xcb_screen_t **screen);

xcb_window_t* find_window_by_pid(xcb_connection_t *dpy,
                                 xcb_window_t window,
                                 const pid_t target_pid);
#endif
