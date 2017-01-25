/* See LICENSE file for copyright and license details. */

/*****************************************************************************
 * Appearance
 ****************************************************************************/

/* fonts */
static const char *fonts[]          = { "Hack-11" };
static const char dmenufont[]       = "Hack-11";

/* colors */
static const char normbordercolor[] = "#3F3F3F"; /* zenburn normal  bg (low contrast) */
static const char normbgcolor[]     = "#3F3F3F"; /* zenburn normal  bg (low contrast) */
static const char normfgcolor[]     = "#DCDCCC"; /* zenburn normal  fg */
static const char selbordercolor[]  = "#8CD0D3"; /* zenburn number  fg */
static const char selbgcolor[]      = "#262626"; /* zenburn cursor  bg */
static const char selfgcolor[]      = "#F0DFAF"; /* zenburn keyword bg */

/* borders */
static const unsigned int borderpx  = 1;         /* border pixel of windows */
static const unsigned int snap      = 32;        /* snap pixel */
static const int showbar            = 1;         /* 0 means no bar */
static const int topbar             = 1;         /* 0 means bottom bar */

/* tagging */
static const char *tags[] = { "www", "text", "term", "server", "music", "chat" };

static const Rule rules[] = {
  /* xprop(1):
   *  WM_CLASS(STRING) = instance, class
   *  WM_NAME(STRING)  = title
   */
  /* class      instance    title       tags mask     isfloating   monitor */
  { "Firefox",  NULL,       NULL,       1 << 8,       0,           -1 },
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
  /* symbol, arrange function */
  { "[]=",   tile },    /* first entry is default */
  { "[M]",   monocle },
  { "><>",   NULL },    /* no layout function means floating behavior */
};

/* key definitions */
#define MODKEY Mod3Mask
#define TAGKEYS(KEY,TAG) \
  { MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
  { MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
  { MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
  { MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0";   /* component of dmenucmd, manipulated in spawn() */

// Terminals
static const char *dmenucmd[]   = { "bash", "-l", "-c", "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL };
static const char *urxvtcmd[]   = { "bash", "-l", "-c", "urxvtc", NULL };

// Editors
static const char *emacscmd[]   = { "bash", "-l", "-c", "emacsclient -c", NULL };
static const char *atomcmd[]    = { "bash", "-l", "-c", "dbus-launch atom", NULL };

// Apps
static const char *chromecmd[]  = { "bash", "-l", "-c", "google-chrome-stable --force-device-scaling=1", NULL };
static const char *spotifycmd[] = { "bash", "-l", "-c", "spotify --force-device-scaling=1", NULL };

// File system
static const char *thunarcmd[]  = { "bash", "-l", "-c", "thunar", NULL };

// Helpers
static const char *incvolcmd[]  = { "bash", "-l", "-c", "amixer -D pulse sset Master 5%+", NULL };
static const char *decvolcmd[]  = { "bash", "-l", "-c", "amixer -D pulse sset Master 5%-", NULL };

static Key keys[] = {
  /* modifier            key              function           argument */
                                                            
  // Terminals                                              
  { MODKEY,              XK_p,            spawn,             {.v = dmenucmd } },
  { MODKEY|ShiftMask,    XK_Return,       spawn,             {.v = urxvtcmd } },
                                                            
  // Editors                                                
  { MODKEY,              XK_e,            spawn,             {.v = emacscmd } },
  { MODKEY,              XK_a,            spawn,             {.v = atomcmd } },
                                                            
  // Apps                                                   
  { MODKEY,              XK_g,            spawn,             {.v = chromecmd } },
  { MODKEY,              XK_s,            spawn,             {.v = spotifycmd } },
                                                            
  // File system                                            
  { MODKEY|ShiftMask,    XK_t,            spawn,             {.v = thunarcmd } },
                                                            
  // Helpers                                                
  { MODKEY,              XK_Page_Up,      spawn,             {.v = incvolcmd } },
  { MODKEY,              XK_Page_Down,    spawn,             {.v = decvolcmd } },
                                                            
  // dwm                                                    
  { MODKEY,              XK_b,            togglebar,         {0} },
  { MODKEY,              XK_j,            focusstack,        {.i = +1 } },
  { MODKEY,              XK_k,            focusstack,        {.i = -1 } },
  { MODKEY,              XK_i,            incnmaster,        {.i = +1 } },
  { MODKEY,              XK_d,            incnmaster,        {.i = -1 } },
  { MODKEY,              XK_h,            setmfact,          {.f = -0.05} },
  { MODKEY,              XK_l,            setmfact,          {.f = +0.05} },
  { MODKEY,              XK_Return,       zoom,              {0} },
  { MODKEY,              XK_Tab,          view,              {0} },
  { MODKEY|ShiftMask,    XK_c,            killclient,        {0} },
  { MODKEY,              XK_t,            setlayout,         {.v = &layouts[0]} },
  { MODKEY,              XK_f,            setlayout,         {.v = &layouts[1]} },
  { MODKEY,              XK_m,            setlayout,         {.v = &layouts[2]} },
  { MODKEY,              XK_space,        setlayout,         {0} },
  { MODKEY|ShiftMask,    XK_space,        togglefloating,    {0} },
  { MODKEY,              XK_0,            view,              {.ui = ~0 } },
  { MODKEY|ShiftMask,    XK_0,            tag,               {.ui = ~0 } },
  { MODKEY,              XK_comma,        focusmon,          {.i = -1 } },
  { MODKEY,              XK_period,       focusmon,          {.i = +1 } },
  { MODKEY|ShiftMask,    XK_comma,        tagmon,            {.i = -1 } },
  { MODKEY|ShiftMask,    XK_period,       tagmon,            {.i = +1 } },
  TAGKEYS(               XK_1,                               0)
  TAGKEYS(               XK_2,                               1)
  TAGKEYS(               XK_3,                               2)
  TAGKEYS(               XK_4,                               3)
  TAGKEYS(               XK_5,                               4)
  TAGKEYS(               XK_6,                               5)
  TAGKEYS(               XK_7,                               6)
  TAGKEYS(               XK_8,                               7)
  TAGKEYS(               XK_9,                               8)
  { MODKEY|ShiftMask,    XK_q,            quit,              {0} },
};

/* button definitions */
/* click can be ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
  /* click                event mask      button          function        argument */
  { ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
  { ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
  { ClkWinTitle,          0,              Button2,        zoom,           {0} },
  { ClkStatusText,        0,              Button2,        spawn,          {.v = urxvtcmd } },
  { ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
  { ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
  { ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
  { ClkTagBar,            0,              Button1,        view,           {0} },
  { ClkTagBar,            0,              Button3,        toggleview,     {0} },
  { ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
  { ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
