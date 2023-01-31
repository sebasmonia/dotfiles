# Dygma Raise configuration

Making some notes about the configuration, for my future self to understand what the hell was the idea. The official nomenclature for the 8-split spacebar is T1~T8, left to right & top to bottom.  
&nbsp;  
In all layers, white keys are for layer swap. I always use "one shot layer" keys, since double tap locks the layer if I need to.  
LED off means the key is unassigned, so I know what I have available at a glance. As I keep using the keyboard and layers, I am sure I will keep refining this setup.
 
## Layer 1 - "Base"

From my MD770 days I got used to having modifiers in my thumbs (Space/Control on the left spacebar, and ESC/Control on the right spacebar), which is very convenient for Emacs. I'm keeping that in the Raise, in T2 and T3.  
T1 is F6, which is bound to my personal keymap in Emacs. I might relocate this key farther from the thumb cluster, depending on how often I reach for it.  
T4 is "switch to layer 5", which is (almost) identical to the base layer, but all keys have the shift modifier. This frees up both shift keys on the sides (still experimenting how to best use them).  
T5, T6 and T7 jump to layer 3 (OS keys), layer 4 (symbols) and layer 2 (function keys and arrows) respectively.  
T8 is DEL. I might move this key to right shift, if I find something that I use more regularly.   
&nbsp;  
I still need proper Alt modifiers for other applications, but since I can trigger Control with thumbs on both halves of the keyboard, I can repurpose the Control positions.   

* Left control: Print screen
* Right control: Pause/Break (For the time being, useful to "escape" from Citrix VDI via Ctrl+Alt+Break, might reassign)
* Caps Lock: Switch keyboard language. Uses a macro that does the shortcut for Linux (Ctrl+Alt+`) and Windows (Alt+Shift) combined.

### Colors

* Underglow: green
* Keys that act as labeled: green
* Keys with dual function tap/hold: orange
* Keys that don't act as labeled: blue
* Modifiers: red 

## Layer 2 - "Function/Arrows & navigation"

This layer is an adjusted version of the layer included by default, but without a NumPad. Haven't had one for quite some time now, I could set it up on the right side if needed.
The arrows keys are right under the home keys (ESDF and IJKL), surrounded by Home/End/PageUp/PageDown on both sides. The number row is for F-keys, so this layer has the same modifiers as Layer 1 plus Shift keys, to allow for Ctrl+F1, Shift+F1, etc.  

### Colors

* Underglow: red
* Keys with dual function tap/hold: orange
* Modifiers: red 
* PageUp/PageDown/Home/End: white
* Arrow keys: yellow
* Function keys: purple

## Layer 3 - "OS/Media/Mouse"

In this layer, the number row has the Win/Super modifier applied. The only reason I still need a Windows key in the base layer is for things like Win+V for clipboard history.
I rely _a lot_ on the Win+# shortcuts to move between applications. I also use Win+arrows in both Gnome and Windows to tile and maximize applications, and to move applications between monitors. These are on the left side just as Layer 2 arrows, plus an extra left/right that adds shift.  
Gnome supports workspaces, mapped on top of the arrow/move windows keys.  
Finally since this layer is pretty empty, and I used the media keys on my MD770 a lot, I am mapping playback and volume to the T# keys not used for layer switching.  
As an experiment, I mapped the mouse on the right side, as arrow keys. Buttons to click are on the sides, middle click below, the scrool wheel on the side. After adjusting the settings for mouse speed and acceleration, this setup has been convenient here and there, so it is staying.  
I also added Alt+F4 in T5, and Win/Super in T8, as quick shortcuts.

### Colors

* Underglow: turquoise
* Win+#: white
* Win+arrow keys (and shifted): yellow
* Win+PgUp/PgDown (and shifted): orange
* Playback: green
* Volume: red
* Keyb LED on/off & screen brightness: blue
* Mouse: purple

## Layer 4 - "Symbols"

Here I am slowly mapping symbols that are useful for programming and moving them to keys close to the home row.

### Colors

* Underglow: yellow
* Keys mapped: yellow

## Layer 5 - "Base + Shift"

This is mostly the same as layer 1 but all the main keys have the shift modifier applied. I haven't had a CapsLock key available for years now (replaced by Ctrl) and don't need one in Emacs since I can M-u my way out of that. But if I lock this layer the effect is very similar to having Caps applied.
