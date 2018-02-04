#ifndef LIB_H
#define LIB_H
#include <xcb/xproto.h>

struct atom_cache_entry {
    xcb_atom_t atom;
    const char *name;
    xcb_intern_atom_cookie_t cookie;
    struct atom_cache_entry *next_entry;
};

struct window_property_selector {
    xcb_window_t window;
    xcb_atom_t property;
    xcb_atom_t type;
    uint32_t length;
};

xcb_atom_t
get_atom_by_name(const char *name);

void*
get_window_property(struct window_property_selector selector);

pid_t
get_window_pid(xcb_window_t window);

void
setup_display_and_screen(const char *display_name);

xcb_window_t*
find_window_by_pid_from_root(const pid_t target_pid);

void
close_xcb_connection();

xcb_generic_event_t *
read_xcb_event();

xcb_generic_event_t *
read_xcb_event_sync();

void
grab_keyboard();

void
ungrab_keyboard();


#endif
