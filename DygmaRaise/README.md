# Dygma Raise configuration

Making some notes about the configuration, for my future self to understand what the hell was the idea. The official nomenclature for the 8-split spacebar is T1~T8, left to right & top to bottom.  
&nbsp;  
In all layers, white keys are for layer swap. I always use "one shot layer" keys, since double tap locks the layer if I need to.  
LED off means the key is unassigned, so I know what I have available at a glance. As I keep using the keyboard and layers, I am sure I will keep refining this setup.
 
## Layer 1 - "Base"

From my MD770 days I got used to having modifiers in my thumbs (Space/Control on the left spacebar, and ESC/Control on the right spacebar), which is very convenient for Emacs.  
I'm keeping that in the Raise (T2 and T3), and adding "Ctrl + Alt" modifiers (T1 and T4), the idea is that I can more easily access Emacs bindings like C-M-SPC (`mark-sexp`). Before, to invoke that command I would tap right space (because ESC->Meta) then hold the same key (Control) and press SPC. Now I can hold T4 and press SPC. Time will tell if this change is worth dedicated keys.  
&nbsp;  
I still need proper Alt modifiers for other applications, but since I can trigger Control with thumbs on both halves of the keyboard, I can repurpose the Control positions.   

* Left control: Print screen
* Right control: Pause/Break (For the time being, useful to "escape" from Citrix VDI via Ctrl+Alt+Break, might reassign)
* Caps Lock: Switch keyboard language. Uses a macro that does quickly the shortcut for Linux and Windows.

I want to reproduce the behaviour of Lisp machines (IIRC) of using a tap on shift on each side to open/close parenthesis, but it doesn't quite work _only in Emacs_...  

### Colors

* Underglow: green
* Keys that act as labeled: green
* Keys with dual function tap/hold: orange
* Keys that don't act as labeled: blue
* Modifiers: red 

## Layer 2 - "Function/Arrows & navigation"

This layer is an adjusted version of the layer included by default, but without a NumPad. Haven't had one for quite some time now, I could set it up on the right side if needed.
The arrows keys are right under the home keys (ESDF and IJKL), surrounded by Home/End/PageUp/PageDown on both sides, symmetrical. The number row is for F-keys, so this layer has the same modifiers as Layer 1 to allow for Ctrl+F1 etc.  

### Colors

* Underglow: red
* Keys with dual function tap/hold: orange
* Modifiers: red 
* PageUp/PageDown/Home/End: white
* Arrow keys: yellow
* Function keys: purple

## Layer 3 - "OS/Media/Mouse"

In this layer, the number row has the Win/Super modifier applied. If the idea sticks, there's potential to free up the Win/Super keys in the base layer.  
I rely _a lot_ on the Win+# shortcuts to move between applications. I also use Win+arrows in both Gnome and Windows to tile and maximize applications, and to move applications between monitors. These are on the left side just as Layer 2 arrows, plus an extra left/right that adds shift.  
Gnome supports workspaces, mapped on top of the arrow/move windows keys.  
Finally since this layer is pretty empty, and I used the media keys on my MD770 a lot, I am mapping playback and volume to the T# keys not used for layer switching.  
EXPERIMENTAL: mapped the mouse on the right side, as arrow keys. Buttons to click are on the sides, middle click below, the scrool wheel on the side.  

### Colors

* Underglow: turquoise? light blue?
* Win+#: white
* Win+arrow keys (and shifted): yellow
* Win+PgUp/PgDown (and shifted): orange
* Playback: green
* Volume: red
* Keyb LED on/off & screen brightness: blue
* Mouse: purple
