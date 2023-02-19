# Dygma Raise configuration

Making some notes about the configuration, for my future self to understand what the hell was the idea. The official nomenclature for the 8-split spacebar is T1~T8, left to right & top to bottom.  
&nbsp;  
In all layers, white keys are for layer swap. I always use "one shot layer" keys. These work as shift to layer too, and if I need to lock a layer I can double tap.  
LED off means the key is unassigned, so I know what I have available at a glance.  
Currently I'm on revision 2 of the setup, changes happened naturally on my first month of use. The most important lesson was to keep the key that I use to activate a layer opposite to the features I use the most without locking on it, in particular for things that aren't a quick tap.
 
## Layer 1 - "Base"

From my MD770 days I got used to having modifiers in my thumbs (Space/Control on the left spacebar, and ESC/Control on the right spacebar), which is very convenient for Emacs. I'm keeping that in the Raise, in T2 and T3.  
T1 is F6, which is bound to my personal keymap in Emacs. I might relocate this key farther from the thumb cluster, depending on how often I reach for it. T4 is DEL.  
The thumbs keys in the bottom row are all layer changes:
  * T5 goes to layer 3, OS/window management, arrow keys, media.
  * T6 goes to layer 4, symbols for programming
  * T7 goes to layer 5, which is almost identical to the base layer, but all keys have the shift modifier. This frees up both shift keys on the sides and reduces chording.
  * T8 goes to layer 2, Function keys, browser/app shorcuts, and mouse keys
&nbsp;  
I still need proper Alt modifiers for other applications, but since I can trigger Control and Shift with thumbs and layer, I can repurpose those keys:   

* Left control: Print screen
* Right control: For the time being, macro to exit Citrix VDI when it is full screen.
* Caps Lock: Switch keyboard language. Uses a macro that does the shortcut for Linux (Ctrl+Alt+\`) and Windows (Alt+Shift) combined.
* Left shift: Prefix argument (C-u) on tap, C-0 on double tab.
* Right shift: Negative argument (C--). Now using `cycle-spacing` to collapse empty lines is a breeze.

![Layer 1](pictures/Layer 1 Base.png)

## Layer 2 - "F-keys/Browse/Mouse"

Since I activate this layer on the right side, it has arrows on left side, but with the Alt modifier applied. They can be used to move between chats in Signal and Webex, for example. In addition, around the arrows there's a couple Firefox-oriented bindings: Ctrl+Shift+Tab and Ctrl+Tab to move between tabs, Ctrl+Shift+T to reopen the last tab, etc.
The right side has mouse keys, which I would use locking the layer (so having both activation and keys on the same side is not as uncomfortable). Mouse buttons to click are on the sides of "up", middle click below, the scrool wheel on the side. After adjusting the settings for mouse speed and acceleration, this setup has been convenient here and there.  
Finally, the number row is for F-keys, so this layer has the same modifiers as Layer 1 plus Shift keys on their usually positions, to allow for Ctrl+F1, Shift+F1, etc.  

![Layer 2](pictures/Layer 2 Fkeys-Browser-Mouse.png)

## Layer 3 - "Windows/Arrows/Media"

In this layer, the number row has the Win/Super modifier applied. The only reason I still need a Windows key in the base layer is for things like Win+V for clipboard history.
I rely _a lot_ on the Win+# shortcuts to move between applications. I also use Win+arrows in both Gnome and Windows to tile and maximize applications, and to move applications between monitors. These are on the left side as ESDF arrows, plus an extra left/right that adds shift. Again, not umcomfortable since I mostly tap these.  
Gnome workspaces shortcuts are mapped on top of the arrow/move windows keys.  
On the right side there are arrow keys mapped to IJKL, surrounded by Home/End/PageUp/PageDown. A lot of times, moving with arrows implies expanding text selections (when not in Emacs, at least) so the layer also has shift and control keys in their usual positions.
Finally since I used the media keys on my MD770 a lot, I am mapping playback and volume in (and around) the T1~4 keys in the top row.  
I also added Alt+F4 in T8, and Win/Super in T6 and T7, as quick shortcuts.

![Layer 3](pictures/Layer 3 OS-Windows-Arrows-Media.png)

## Layer 4 - "Symbols"

Here I mapped symbols that are useful for programming and moved them to keys close to the home row. Braces, parenthesis (obviously), math symbols, all types of quotes, etc.

![Layer 4](pictures/Layer 4 Symbols.png)

## Layer 5 - "Base + Shift"

This is mostly the same as layer 1 but all the main keys have the shift modifier applied. I haven't had a CapsLock key available for years now (replaced by Ctrl) and don't need one in Emacs since I can M-u my way out of that. But if I lock this layer the effect is very similar to having Caps applied, for other applications.

![Layer 5](pictures/Layer 5 Base w Shift.png)
