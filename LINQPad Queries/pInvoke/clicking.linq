<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
</Query>

open System.Drawing
module PInvoke =
    open System.Runtime.InteropServices
    
    [<DllImport("user32.dll")>]
    extern IntPtr private SendMessage(IntPtr hWnd, int Msg,IntPtr wParam, IntPtr lParam);

    [<DllImport("user32.dll", EntryPoint = "WindowFromPoint",CharSet = CharSet.Auto, ExactSpelling = true)>]
    extern IntPtr private WindowFromPoint(Point point);
    
    module WindowsMessages = // from https://www.pinvoke.net/default.aspx/Enums/WindowsMessages.html

        /// Windows Messages
        /// Defined in winuser.h from Windows SDK v6.1
        /// Documentation pulled from MSDN.


        /// The WM_NULL message performs no operation. An application sends the WM_NULL message if it wants to post a message that the recipient window will ignore.

        let NULL = 0x0000

        /// The WM_CREATE message is sent when an application requests that a window be created by calling the CreateWindowEx or CreateWindow function. (The message is sent before the function returns.) The window procedure of the new window receives this message after the window is created, but before the window becomes visible.

        let CREATE = 0x0001

        /// The WM_DESTROY message is sent when a window is being destroyed. It is sent to the window procedure of the window being destroyed after the window is removed from the screen. 
        /// This message is sent first to the window being destroyed and then to the child windows (if any) as they are destroyed. During the processing of the message, it can be assumed that all child windows still exist.

        let DESTROY = 0x0002

        /// The WM_MOVE message is sent after a window has been moved. 

        let MOVE = 0x0003

        /// The WM_SIZE message is sent to a window after its size has changed.

        let SIZE = 0x0005

        /// The WM_ACTIVATE message is sent to both the window being activated and the window being deactivated. If the windows use the same input queue, the message is sent synchronously, first to the window procedure of the top-level window being deactivated, then to the window procedure of the top-level window being activated. If the windows use different input queues, the message is sent asynchronously, so the window is activated immediately. 

        let ACTIVATE = 0x0006

        /// The WM_SETFOCUS message is sent to a window after it has gained the keyboard focus. 

        let SETFOCUS = 0x0007

        /// The WM_KILLFOCUS message is sent to a window immediately before it loses the keyboard focus. 

        let KILLFOCUS = 0x0008

        /// The WM_ENABLE message is sent when an application changes the enabled state of a window. It is sent to the window whose enabled state is changing. This message is sent before the EnableWindow function returns, but after the enabled state (WS_DISABLED style bit) of the window has changed. 

        let ENABLE = 0x000A

        /// An application sends the WM_SETREDRAW message to a window to allow changes in that window to be redrawn or to prevent changes in that window from being redrawn. 

        let SETREDRAW = 0x000B

        /// An application sends a WM_SETTEXT message to set the text of a window. 

        let SETTEXT = 0x000C

        /// An application sends a WM_GETTEXT message to copy the text that corresponds to a window into a buffer provided by the caller. 

        let GETTEXT = 0x000D

        /// An application sends a WM_GETTEXTLENGTH message to determine the length, in characters, of the text associated with a window. 

        let GETTEXTLENGTH = 0x000E

        /// The WM_PAINT message is sent when the system or another application makes a request to paint a portion of an application's window. The message is sent when the UpdateWindow or RedrawWindow function is called, or by the DispatchMessage function when the application obtains a WM_PAINT message by using the GetMessage or PeekMessage function. 

        let PAINT = 0x000F

        /// The WM_CLOSE message is sent as a signal that a window or an application should terminate.

        let CLOSE = 0x0010

        /// The WM_QUERYENDSESSION message is sent when the user chooses to end the session or when an application calls one of the system shutdown functions. If any application returns zero, the session is not ended. The system stops sending WM_QUERYENDSESSION messages as soon as one application returns zero.
        /// After processing this message, the system sends the WM_ENDSESSION message with the wParam parameter set to the results of the WM_QUERYENDSESSION message.

        let QUERYENDSESSION = 0x0011

        /// The WM_QUERYOPEN message is sent to an icon when the user requests that the window be restored to its previous size and position.

        let QUERYOPEN = 0x0013

        /// The WM_ENDSESSION message is sent to an application after the system processes the results of the WM_QUERYENDSESSION message. The WM_ENDSESSION message informs the application whether the session is ending.

        let ENDSESSION = 0x0016

        /// The WM_QUIT message indicates a request to terminate an application and is generated when the application calls the PostQuitMessage function. It causes the GetMessage function to return zero.

        let QUIT = 0x0012

        /// The WM_ERASEBKGND message is sent when the window background must be erased (for example, when a window is resized). The message is sent to prepare an invalidated portion of a window for painting. 

        let ERASEBKGND = 0x0014

        /// This message is sent to all top-level windows when a change is made to a system color setting. 

        let SYSCOLORCHANGE = 0x0015

        /// The WM_SHOWWINDOW message is sent to a window when the window is about to be hidden or shown.

        let SHOWWINDOW = 0x0018

        /// An application sends the WM_WININICHANGE message to all top-level windows after making a change to the WIN.INI file. The SystemParametersInfo function sends this message after an application uses the function to change a setting in WIN.INI.
        /// Note  The WM_WININICHANGE message is provided only for compatibility with earlier versions of the system. Applications should use the WM_SETTINGCHANGE message.

        let WININICHANGE = 0x001A

        /// An application sends the WM_WININICHANGE message to all top-level windows after making a change to the WIN.INI file. The SystemParametersInfo function sends this message after an application uses the function to change a setting in WIN.INI.
        /// Note  The WM_WININICHANGE message is provided only for compatibility with earlier versions of the system. Applications should use the WM_SETTINGCHANGE message.

        let SETTINGCHANGE = WININICHANGE

        /// The WM_DEVMODECHANGE message is sent to all top-level windows whenever the user changes device-mode settings. 

        let DEVMODECHANGE = 0x001B

        /// The WM_ACTIVATEAPP message is sent when a window belonging to a different application than the active window is about to be activated. The message is sent to the application whose window is being activated and to the application whose window is being deactivated.

        let ACTIVATEAPP = 0x001C

        /// An application sends the WM_FONTCHANGE message to all top-level windows in the system after changing the pool of font resources. 

        let FONTCHANGE = 0x001D

        /// A message that is sent whenever there is a change in the system time.

        let TIMECHANGE = 0x001E

        /// The WM_CANCELMODE message is sent to cancel certain modes, such as mouse capture. For example, the system sends this message to the active window when a dialog box or message box is displayed. Certain functions also send this message explicitly to the specified window regardless of whether it is the active window. For example, the EnableWindow function sends this message when disabling the specified window.

        let CANCELMODE = 0x001F

        /// The WM_SETCURSOR message is sent to a window if the mouse causes the cursor to move within a window and mouse input is not captured. 

        let SETCURSOR = 0x0020

        /// The WM_MOUSEACTIVATE message is sent when the cursor is in an inactive window and the user presses a mouse button. The parent window receives this message only if the child window passes it to the DefWindowProc function.

        let MOUSEACTIVATE = 0x0021

        /// The WM_CHILDACTIVATE message is sent to a child window when the user clicks the window's title bar or when the window is activated, moved, or sized.

        let CHILDACTIVATE = 0x0022

        /// The WM_QUEUESYNC message is sent by a computer-based training (CBT) application to separate user-input messages from other messages sent through the WH_JOURNALPLAYBACK Hook procedure. 

        let QUEUESYNC = 0x0023

        /// The WM_GETMINMAXINFO message is sent to a window when the size or position of the window is about to change. An application can use this message to override the window's default maximized size and position, or its default minimum or maximum tracking size. 

        let GETMINMAXINFO = 0x0024

        /// Windows NT 3.51 and earlier: The WM_PAINTICON message is sent to a minimized window when the icon is to be painted. This message is not sent by newer versions of Microsoft Windows, except in unusual circumstances explained in the Remarks.

        let PAINTICON = 0x0026

        /// Windows NT 3.51 and earlier: The WM_ICONERASEBKGND message is sent to a minimized window when the background of the icon must be filled before painting the icon. A window receives this message only if a class icon is defined for the window; otherwise, WM_ERASEBKGND is sent. This message is not sent by newer versions of Windows.

        let ICONERASEBKGND = 0x0027

        /// The WM_NEXTDLGCTL message is sent to a dialog box procedure to set the keyboard focus to a different control in the dialog box. 

        let NEXTDLGCTL = 0x0028

        /// The WM_SPOOLERSTATUS message is sent from Print Manager whenever a job is added to or removed from the Print Manager queue. 

        let SPOOLERSTATUS = 0x002A
        
        /// The WM_DRAWITEM message is sent to the parent window of an owner-drawn button, combo box, list box, or menu when a visual aspect of the button, combo box, list box, or menu has changed.

        let DRAWITEM = 0x002B

        /// The WM_MEASUREITEM message is sent to the owner window of a combo box, list box, list view control, or menu item when the control or menu is created.

        let MEASUREITEM = 0x002C

        /// Sent to the owner of a list box or combo box when the list box or combo box is destroyed or when items are removed by the LB_DELETESTRING, LB_RESETCONTENT, CB_DELETESTRING, or CB_RESETCONTENT message. The system sends a WM_DELETEITEM message for each deleted item. The system sends the WM_DELETEITEM message for any deleted list box or combo box item with nonzero item data.

        let DELETEITEM = 0x002D

        /// Sent by a list box with the LBS_WANTKEYBOARDINPUT style to its owner in response to a WM_KEYDOWN message. 

        let VKEYTOITEM = 0x002E

        /// Sent by a list box with the LBS_WANTKEYBOARDINPUT style to its owner in response to a WM_CHAR message. 

        let CHARTOITEM = 0x002F

        /// An application sends a WM_SETFONT message to specify the font that a control is to use when drawing text. 

        let SETFONT = 0x0030

        /// An application sends a WM_GETFONT message to a control to retrieve the font with which the control is currently drawing its text. 

        let GETFONT = 0x0031

        /// An application sends a WM_SETHOTKEY message to a window to associate a hot key with the window. When the user presses the hot key, the system activates the window. 

        let SETHOTKEY = 0x0032

        /// An application sends a WM_GETHOTKEY message to determine the hot key associated with a window. 

        let GETHOTKEY = 0x0033

        /// The WM_QUERYDRAGICON message is sent to a minimized (iconic) window. The window is about to be dragged by the user but does not have an icon defined for its class. An application can return a handle to an icon or cursor. The system displays this cursor or icon while the user drags the icon.

        let QUERYDRAGICON = 0x0037

        /// The system sends the WM_COMPAREITEM message to determine the relative position of a new item in the sorted list of an owner-drawn combo box or list box. Whenever the application adds a new item, the system sends this message to the owner of a combo box or list box created with the CBS_SORT or LBS_SORT style. 

        let COMPAREITEM = 0x0039

        /// Active Accessibility sends the WM_GETOBJECT message to obtain information about an accessible object contained in a server application. 
        /// Applications never send this message directly. It is sent only by Active Accessibility in response to calls to AccessibleObjectFromPoint, AccessibleObjectFromEvent, or AccessibleObjectFromWindow. However, server applications handle this message. 

        let GETOBJECT = 0x003D

        /// The WM_COMPACTING message is sent to all top-level windows when the system detects more than 12.5 percent of system time over a 30- to 60-second interval is being spent compacting memory. This indicates that system memory is low.

        let COMPACTING = 0x0041

        /// WM_COMMNOTIFY is Obsolete for Win32-Based Applications

        [<Obsolete>]
        let COMMNOTIFY = 0x0044

        /// The WM_WINDOWPOSCHANGING message is sent to a window whose size, position, or place in the Z order is about to change as a result of a call to the SetWindowPos function or another window-management function.

        let WINDOWPOSCHANGING = 0x0046

        /// The WM_WINDOWPOSCHANGED message is sent to a window whose size, position, or place in the Z order has changed as a result of a call to the SetWindowPos function or another window-management function.

        let WINDOWPOSCHANGED = 0x0047

        /// Notifies applications that the system, typically a battery-powered personal computer, is about to enter a suspended mode.
        /// Use: POWERBROADCAST

        [<Obsolete>]
        let POWER = 0x0048

        /// An application sends the WM_COPYDATA message to pass data to another application. 

        let COPYDATA = 0x004A

        /// The WM_CANCELJOURNAL message is posted to an application when a user cancels the application's journaling activities. The message is posted with a NULL window handle. 

        let CANCELJOURNAL = 0x004B

        /// Sent by a common control to its parent window when an event has occurred or the control requires some information. 

        let NOTIFY = 0x004E

        /// The WM_INPUTLANGCHANGEREQUEST message is posted to the window with the focus when the user chooses a new input language, either with the hotkey (specified in the Keyboard control panel application) or from the indicator on the system taskbar. An application can accept the change by passing the message to the DefWindowProc function or reject the change (and prevent it from taking place) by returning immediately. 

        let INPUTLANGCHANGEREQUEST = 0x0050

        /// The WM_INPUTLANGCHANGE message is sent to the topmost affected window after an application's input language has been changed. You should make any application-specific settings and pass the message to the DefWindowProc function, which passes the message to all first-level child windows. These child windows can pass the message to DefWindowProc to have it pass the message to their child windows, and so on. 

        let INPUTLANGCHANGE = 0x0051

        /// Sent to an application that has initiated a training card with Microsoft Windows Help. The message informs the application when the user clicks an authorable button. An application initiates a training card by specifying the HELP_TCARD command in a call to the WinHelp function.

        let TCARD = 0x0052

        /// Indicates that the user pressed the F1 key. If a menu is active when F1 is pressed, WM_HELP is sent to the window associated with the menu; otherwise, WM_HELP is sent to the window that has the keyboard focus. If no window has the keyboard focus, WM_HELP is sent to the currently active window. 

        let HELP = 0x0053

        /// The WM_USERCHANGED message is sent to all windows after the user has logged on or off. When the user logs on or off, the system updates the user-specific settings. The system sends this message immediately after updating the settings.

        let USERCHANGED = 0x0054

        /// Determines if a window accepts ANSI or Unicode structures in the WM_NOTIFY notification message. WM_NOTIFYFORMAT messages are sent from a common control to its parent window and from the parent window to the common control.

        let NOTIFYFORMAT = 0x0055

        /// The WM_CONTEXTMENU message notifies a window that the user clicked the right mouse button (right-clicked) in the window.

        let CONTEXTMENU = 0x007B

        /// The WM_STYLECHANGING message is sent to a window when the SetWindowLong function is about to change one or more of the window's styles.

        let STYLECHANGING = 0x007C

        /// The WM_STYLECHANGED message is sent to a window after the SetWindowLong function has changed one or more of the window's styles

        let STYLECHANGED = 0x007D

        /// The WM_DISPLAYCHANGE message is sent to all windows when the display resolution has changed.

        let DISPLAYCHANGE = 0x007E

        /// The WM_GETICON message is sent to a window to retrieve a handle to the large or small icon associated with a window. The system displays the large icon in the ALT+TAB dialog, and the small icon in the window caption. 

        let GETICON = 0x007F

        /// An application sends the WM_SETICON message to associate a new large or small icon with a window. The system displays the large icon in the ALT+TAB dialog box, and the small icon in the window caption. 

        let SETICON = 0x0080

        /// The WM_NCCREATE message is sent prior to the WM_CREATE message when a window is first created.

        let NCCREATE = 0x0081

        /// The WM_NCDESTROY message informs a window that its nonclient area is being destroyed. The DestroyWindow function sends the WM_NCDESTROY message to the window following the WM_DESTROY message. WM_DESTROY is used to free the allocated memory object associated with the window. 
        /// The WM_NCDESTROY message is sent after the child windows have been destroyed. In contrast, WM_DESTROY is sent before the child windows are destroyed.

        let NCDESTROY = 0x0082

        /// The WM_NCCALCSIZE message is sent when the size and position of a window's client area must be calculated. By processing this message, an application can control the content of the window's client area when the size or position of the window changes.

        let NCCALCSIZE = 0x0083

        /// The WM_NCHITTEST message is sent to a window when the cursor moves, or when a mouse button is pressed or released. If the mouse is not captured, the message is sent to the window beneath the cursor. Otherwise, the message is sent to the window that has captured the mouse.

        let NCHITTEST = 0x0084

        /// The WM_NCPAINT message is sent to a window when its frame must be painted. 

        let NCPAINT = 0x0085

        /// The WM_NCACTIVATE message is sent to a window when its nonclient area needs to be changed to indicate an active or inactive state.

        let NCACTIVATE = 0x0086

        /// The WM_GETDLGCODE message is sent to the window procedure associated with a control. By default, the system handles all keyboard input to the control; the system interprets certain types of keyboard input as dialog box navigation keys. To override this default behavior, the control can respond to the WM_GETDLGCODE message to indicate the types of input it wants to process itself.

        let GETDLGCODE = 0x0087

        /// The WM_SYNCPAINT message is used to synchronize painting while avoiding linking independent GUI threads.

        let SYNCPAINT = 0x0088

        /// The WM_NCMOUSEMOVE message is posted to a window when the cursor is moved within the nonclient area of the window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCMOUSEMOVE = 0x00A0

        /// The WM_NCLBUTTONDOWN message is posted when the user presses the left mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCLBUTTONDOWN = 0x00A1

        /// The WM_NCLBUTTONUP message is posted when the user releases the left mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCLBUTTONUP = 0x00A2

        /// The WM_NCLBUTTONDBLCLK message is posted when the user double-clicks the left mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCLBUTTONDBLCLK = 0x00A3

        /// The WM_NCRBUTTONDOWN message is posted when the user presses the right mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCRBUTTONDOWN = 0x00A4

        /// The WM_NCRBUTTONUP message is posted when the user releases the right mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCRBUTTONUP = 0x00A5

        /// The WM_NCRBUTTONDBLCLK message is posted when the user double-clicks the right mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCRBUTTONDBLCLK = 0x00A6

        /// The WM_NCMBUTTONDOWN message is posted when the user presses the middle mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCMBUTTONDOWN = 0x00A7

        /// The WM_NCMBUTTONUP message is posted when the user releases the middle mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCMBUTTONUP = 0x00A8

        /// The WM_NCMBUTTONDBLCLK message is posted when the user double-clicks the middle mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCMBUTTONDBLCLK = 0x00A9

        /// The WM_NCXBUTTONDOWN message is posted when the user presses the first or second X button while the cursor is in the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCXBUTTONDOWN = 0x00AB

        /// The WM_NCXBUTTONUP message is posted when the user releases the first or second X button while the cursor is in the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCXBUTTONUP = 0x00AC

        /// The WM_NCXBUTTONDBLCLK message is posted when the user double-clicks the first or second X button while the cursor is in the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.

        let NCXBUTTONDBLCLK = 0x00AD

        /// The WM_INPUT_DEVICE_CHANGE message is sent to the window that registered to receive raw input. A window receives this message through its WindowProc function.

        let INPUT_DEVICE_CHANGE = 0x00FE

        /// The WM_INPUT message is sent to the window that is getting raw input. 

        let INPUT = 0x00FF

        /// This message filters for keyboard messages.

        let KEYFIRST = 0x0100

        /// The WM_KEYDOWN message is posted to the window with the keyboard focus when a nonsystem key is pressed. A nonsystem key is a key that is pressed when the ALT key is not pressed. 

        let KEYDOWN = 0x0100

        /// The WM_KEYUP message is posted to the window with the keyboard focus when a nonsystem key is released. A nonsystem key is a key that is pressed when the ALT key is not pressed, or a keyboard key that is pressed when a window has the keyboard focus. 

        let KEYUP = 0x0101

        /// The WM_CHAR message is posted to the window with the keyboard focus when a WM_KEYDOWN message is translated by the TranslateMessage function. The WM_CHAR message contains the character code of the key that was pressed. 

        let CHAR = 0x0102

        /// The WM_DEADCHAR message is posted to the window with the keyboard focus when a WM_KEYUP message is translated by the TranslateMessage function. WM_DEADCHAR specifies a character code generated by a dead key. A dead key is a key that generates a character, such as the umlaut (double-dot), that is combined with another character to form a composite character. For example, the umlaut-O character (Ö) is generated by typing the dead key for the umlaut character, and then typing the O key. 

        let DEADCHAR = 0x0103

        /// The WM_SYSKEYDOWN message is posted to the window with the keyboard focus when the user presses the F10 key (which activates the menu bar) or holds down the ALT key and then presses another key. It also occurs when no window currently has the keyboard focus; in this case, the WM_SYSKEYDOWN message is sent to the active window. The window that receives the message can distinguish between these two contexts by checking the context code in the lParam parameter. 

        let SYSKEYDOWN = 0x0104

        /// The WM_SYSKEYUP message is posted to the window with the keyboard focus when the user releases a key that was pressed while the ALT key was held down. It also occurs when no window currently has the keyboard focus; in this case, the WM_SYSKEYUP message is sent to the active window. The window that receives the message can distinguish between these two contexts by checking the context code in the lParam parameter. 

        let SYSKEYUP = 0x0105

        /// The WM_SYSCHAR message is posted to the window with the keyboard focus when a WM_SYSKEYDOWN message is translated by the TranslateMessage function. It specifies the character code of a system character key — that is, a character key that is pressed while the ALT key is down. 

        let SYSCHAR = 0x0106

        /// The WM_SYSDEADCHAR message is sent to the window with the keyboard focus when a WM_SYSKEYDOWN message is translated by the TranslateMessage function. WM_SYSDEADCHAR specifies the character code of a system dead key — that is, a dead key that is pressed while holding down the ALT key. 

        let SYSDEADCHAR = 0x0107

        /// The WM_UNICHAR message is posted to the window with the keyboard focus when a WM_KEYDOWN message is translated by the TranslateMessage function. The WM_UNICHAR message contains the character code of the key that was pressed. 
        /// The WM_UNICHAR message is equivalent to WM_CHAR, but it uses Unicode Transformation Format (UTF)-32, whereas WM_CHAR uses UTF-16. It is designed to send or post Unicode characters to ANSI windows and it can can handle Unicode Supplementary Plane characters.

        let UNICHAR = 0x0109

        /// This message filters for keyboard messages.

        let KEYLAST = 0x0108

        /// Sent immediately before the IME generates the composition string as a result of a keystroke. A window receives this message through its WindowProc function. 

        let IME_STARTCOMPOSITION = 0x010D

        /// Sent to an application when the IME ends composition. A window receives this message through its WindowProc function. 

        let IME_ENDCOMPOSITION = 0x010E

        /// Sent to an application when the IME changes composition status as a result of a keystroke. A window receives this message through its WindowProc function. 

        let IME_COMPOSITION = 0x010F
        let IME_KEYLAST = 0x010F

        /// The WM_INITDIALOG message is sent to the dialog box procedure immediately before a dialog box is displayed. Dialog box procedures typically use this message to initialize controls and carry out any other initialization tasks that affect the appearance of the dialog box. 

        let INITDIALOG = 0x0110

        /// The WM_COMMAND message is sent when the user selects a command item from a menu, when a control sends a notification message to its parent window, or when an accelerator keystroke is translated. 

        let COMMAND = 0x0111

        /// A window receives this message when the user chooses a command from the Window menu, clicks the maximize button, minimize button, restore button, close button, or moves the form. You can stop the form from moving by filtering this out.

        let SYSCOMMAND = 0x0112

        /// The WM_TIMER message is posted to the installing thread's message queue when a timer expires. The message is posted by the GetMessage or PeekMessage function. 

        let TIMER = 0x0113

        /// The WM_HSCROLL message is sent to a window when a scroll event occurs in the window's standard horizontal scroll bar. This message is also sent to the owner of a horizontal scroll bar control when a scroll event occurs in the control. 

        let HSCROLL = 0x0114

        /// The WM_VSCROLL message is sent to a window when a scroll event occurs in the window's standard vertical scroll bar. This message is also sent to the owner of a vertical scroll bar control when a scroll event occurs in the control. 

        let VSCROLL = 0x0115

        /// The WM_INITMENU message is sent when a menu is about to become active. It occurs when the user clicks an item on the menu bar or presses a menu key. This allows the application to modify the menu before it is displayed. 

        let INITMENU = 0x0116

        /// The WM_INITMENUPOPUP message is sent when a drop-down menu or submenu is about to become active. This allows an application to modify the menu before it is displayed, without changing the entire menu. 

        let INITMENUPOPUP = 0x0117

        /// The WM_MENUSELECT message is sent to a menu's owner window when the user selects a menu item. 

        let MENUSELECT = 0x011F

        /// The WM_MENUCHAR message is sent when a menu is active and the user presses a key that does not correspond to any mnemonic or accelerator key. This message is sent to the window that owns the menu. 

        let MENUCHAR = 0x0120

        /// The WM_ENTERIDLE message is sent to the owner window of a modal dialog box or menu that is entering an idle state. A modal dialog box or menu enters an idle state when no messages are waiting in its queue after it has processed one or more previous messages. 

        let ENTERIDLE = 0x0121

        /// The WM_MENURBUTTONUP message is sent when the user releases the right mouse button while the cursor is on a menu item. 

        let MENURBUTTONUP = 0x0122

        /// The WM_MENUDRAG message is sent to the owner of a drag-and-drop menu when the user drags a menu item. 

        let MENUDRAG = 0x0123

        /// The WM_MENUGETOBJECT message is sent to the owner of a drag-and-drop menu when the mouse cursor enters a menu item or moves from the center of the item to the top or bottom of the item. 

        let MENUGETOBJECT = 0x0124

        /// The WM_UNINITMENUPOPUP message is sent when a drop-down menu or submenu has been destroyed. 

        let UNINITMENUPOPUP = 0x0125

        /// The WM_MENUCOMMAND message is sent when the user makes a selection from a menu. 

        let MENUCOMMAND = 0x0126

        /// An application sends the WM_CHANGEUISTATE message to indicate that the user interface (UI) state should be changed.

        let CHANGEUISTATE = 0x0127

        /// An application sends the WM_UPDATEUISTATE message to change the user interface (UI) state for the specified window and all its child windows.

        let UPDATEUISTATE = 0x0128

        /// An application sends the WM_QUERYUISTATE message to retrieve the user interface (UI) state for a window.

        let QUERYUISTATE = 0x0129

        /// The WM_CTLCOLORMSGBOX message is sent to the owner window of a message box before Windows draws the message box. By responding to this message, the owner window can set the text and background colors of the message box by using the given display device context handle. 

        let CTLCOLORMSGBOX = 0x0132

        /// An edit control that is not read-only or disabled sends the WM_CTLCOLOREDIT message to its parent window when the control is about to be drawn. By responding to this message, the parent window can use the specified device context handle to set the text and background colors of the edit control. 

        let CTLCOLOREDIT = 0x0133

        /// Sent to the parent window of a list box before the system draws the list box. By responding to this message, the parent window can set the text and background colors of the list box by using the specified display device context handle. 

        let CTLCOLORLISTBOX = 0x0134

        /// The WM_CTLCOLORBTN message is sent to the parent window of a button before drawing the button. The parent window can change the button's text and background colors. However, only owner-drawn buttons respond to the parent window processing this message. 

        let CTLCOLORBTN = 0x0135

        /// The WM_CTLCOLORDLG message is sent to a dialog box before the system draws the dialog box. By responding to this message, the dialog box can set its text and background colors using the specified display device context handle. 

        let CTLCOLORDLG = 0x0136

        /// The WM_CTLCOLORSCROLLBAR message is sent to the parent window of a scroll bar control when the control is about to be drawn. By responding to this message, the parent window can use the display context handle to set the background color of the scroll bar control. 

        let CTLCOLORSCROLLBAR = 0x0137

        /// A static control, or an edit control that is read-only or disabled, sends the WM_CTLCOLORSTATIC message to its parent window when the control is about to be drawn. By responding to this message, the parent window can use the specified device context handle to set the text and background colors of the static control. 

        let CTLCOLORSTATIC = 0x0138

        /// Use WM_MOUSEFIRST to specify the first mouse message. Use the PeekMessage() Function.

        let MOUSEFIRST = 0x0200

        /// The WM_MOUSEMOVE message is posted to a window when the cursor moves. If the mouse is not captured, the message is posted to the window that contains the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let MOUSEMOVE = 0x0200

        /// The WM_LBUTTONDOWN message is posted when the user presses the left mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let LBUTTONDOWN = 0x0201

        /// The WM_LBUTTONUP message is posted when the user releases the left mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let LBUTTONUP = 0x0202

        /// The WM_LBUTTONDBLCLK message is posted when the user double-clicks the left mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let LBUTTONDBLCLK = 0x0203

        /// The WM_RBUTTONDOWN message is posted when the user presses the right mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let RBUTTONDOWN = 0x0204

        /// The WM_RBUTTONUP message is posted when the user releases the right mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let RBUTTONUP = 0x0205

        /// The WM_RBUTTONDBLCLK message is posted when the user double-clicks the right mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let RBUTTONDBLCLK = 0x0206

        /// The WM_MBUTTONDOWN message is posted when the user presses the middle mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let MBUTTONDOWN = 0x0207

        /// The WM_MBUTTONUP message is posted when the user releases the middle mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let MBUTTONUP = 0x0208

        /// The WM_MBUTTONDBLCLK message is posted when the user double-clicks the middle mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let MBUTTONDBLCLK = 0x0209

        /// The WM_MOUSEWHEEL message is sent to the focus window when the mouse wheel is rotated. The DefWindowProc function propagates the message to the window's parent. There should be no internal forwarding of the message, since DefWindowProc propagates it up the parent chain until it finds a window that processes it.

        let MOUSEWHEEL = 0x020A

        /// The WM_XBUTTONDOWN message is posted when the user presses the first or second X button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse. 

        let XBUTTONDOWN = 0x020B

        /// The WM_XBUTTONUP message is posted when the user releases the first or second X button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let XBUTTONUP = 0x020C

        /// The WM_XBUTTONDBLCLK message is posted when the user double-clicks the first or second X button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.

        let XBUTTONDBLCLK = 0x020D

        /// The WM_MOUSEHWHEEL message is sent to the focus window when the mouse's horizontal scroll wheel is tilted or rotated. The DefWindowProc function propagates the message to the window's parent. There should be no internal forwarding of the message, since DefWindowProc propagates it up the parent chain until it finds a window that processes it.

        let MOUSEHWHEEL = 0x020E

        /// Use WM_MOUSELAST to specify the last mouse message. Used with PeekMessage() Function.

        let MOUSELAST = 0x020E

        /// The WM_PARENTNOTIFY message is sent to the parent of a child window when the child window is created or destroyed, or when the user clicks a mouse button while the cursor is over the child window. When the child window is being created, the system sends WM_PARENTNOTIFY just before the CreateWindow or CreateWindowEx function that creates the window returns. When the child window is being destroyed, the system sends the message before any processing to destroy the window takes place.

        let PARENTNOTIFY = 0x0210

        /// The WM_ENTERMENULOOP message informs an application's main window procedure that a menu modal loop has been entered. 

        let ENTERMENULOOP = 0x0211

        /// The WM_EXITMENULOOP message informs an application's main window procedure that a menu modal loop has been exited. 

        let EXITMENULOOP = 0x0212

        /// The WM_NEXTMENU message is sent to an application when the right or left arrow key is used to switch between the menu bar and the system menu. 

        let NEXTMENU = 0x0213

        /// The WM_SIZING message is sent to a window that the user is resizing. By processing this message, an application can monitor the size and position of the drag rectangle and, if needed, change its size or position. 

        let SIZING = 0x0214

        /// The WM_CAPTURECHANGED message is sent to the window that is losing the mouse capture.

        let CAPTURECHANGED = 0x0215

        /// The WM_MOVING message is sent to a window that the user is moving. By processing this message, an application can monitor the position of the drag rectangle and, if needed, change its position.

        let MOVING = 0x0216

        /// Notifies applications that a power-management event has occurred.

        let POWERBROADCAST = 0x0218

        /// Notifies an application of a change to the hardware configuration of a device or the computer.

        let DEVICECHANGE = 0x0219

        /// An application sends the WM_MDICREATE message to a multiple-document interface (MDI) client window to create an MDI child window. 

        let MDICREATE = 0x0220

        /// An application sends the WM_MDIDESTROY message to a multiple-document interface (MDI) client window to close an MDI child window. 

        let MDIDESTROY = 0x0221

        /// An application sends the WM_MDIACTIVATE message to a multiple-document interface (MDI) client window to instruct the client window to activate a different MDI child window. 

        let MDIACTIVATE = 0x0222

        /// An application sends the WM_MDIRESTORE message to a multiple-document interface (MDI) client window to restore an MDI child window from maximized or minimized size. 

        let MDIRESTORE = 0x0223

        /// An application sends the WM_MDINEXT message to a multiple-document interface (MDI) client window to activate the next or previous child window. 

        let MDINEXT = 0x0224

        /// An application sends the WM_MDIMAXIMIZE message to a multiple-document interface (MDI) client window to maximize an MDI child window. The system resizes the child window to make its client area fill the client window. The system places the child window's window menu icon in the rightmost position of the frame window's menu bar, and places the child window's restore icon in the leftmost position. The system also appends the title bar text of the child window to that of the frame window. 

        let MDIMAXIMIZE = 0x0225

        /// An application sends the WM_MDITILE message to a multiple-document interface (MDI) client window to arrange all of its MDI child windows in a tile format. 

        let MDITILE = 0x0226

        /// An application sends the WM_MDICASCADE message to a multiple-document interface (MDI) client window to arrange all its child windows in a cascade format. 

        let MDICASCADE = 0x0227

        /// An application sends the WM_MDIICONARRANGE message to a multiple-document interface (MDI) client window to arrange all minimized MDI child windows. It does not affect child windows that are not minimized. 

        let MDIICONARRANGE = 0x0228

        /// An application sends the WM_MDIGETACTIVE message to a multiple-document interface (MDI) client window to retrieve the handle to the active MDI child window. 

        let MDIGETACTIVE = 0x0229

        /// An application sends the WM_MDISETMENU message to a multiple-document interface (MDI) client window to replace the entire menu of an MDI frame window, to replace the window menu of the frame window, or both. 

        let MDISETMENU = 0x0230

        /// The WM_ENTERSIZEMOVE message is sent one time to a window after it enters the moving or sizing modal loop. The window enters the moving or sizing modal loop when the user clicks the window's title bar or sizing border, or when the window passes the WM_SYSCOMMAND message to the DefWindowProc function and the wParam parameter of the message specifies the SC_MOVE or SC_SIZE value. The operation is complete when DefWindowProc returns. 
        /// The system sends the WM_ENTERSIZEMOVE message regardless of whether the dragging of full windows is enabled.

        let ENTERSIZEMOVE = 0x0231

        /// The WM_EXITSIZEMOVE message is sent one time to a window, after it has exited the moving or sizing modal loop. The window enters the moving or sizing modal loop when the user clicks the window's title bar or sizing border, or when the window passes the WM_SYSCOMMAND message to the DefWindowProc function and the wParam parameter of the message specifies the SC_MOVE or SC_SIZE value. The operation is complete when DefWindowProc returns. 

        let EXITSIZEMOVE = 0x0232

        /// Sent when the user drops a file on the window of an application that has registered itself as a recipient of dropped files.

        let DROPFILES = 0x0233

        /// An application sends the WM_MDIREFRESHMENU message to a multiple-document interface (MDI) client window to refresh the window menu of the MDI frame window. 

        let MDIREFRESHMENU = 0x0234

        /// Sent to an application when a window is activated. A window receives this message through its WindowProc function. 

        let IME_SETCONTEXT = 0x0281

        /// Sent to an application to notify it of changes to the IME window. A window receives this message through its WindowProc function. 

        let IME_NOTIFY = 0x0282

        /// Sent by an application to direct the IME window to carry out the requested command. The application uses this message to control the IME window that it has created. To send this message, the application calls the SendMessage function with the following parameters.

        let IME_CONTROL = 0x0283

        /// Sent to an application when the IME window finds no space to extend the area for the composition window. A window receives this message through its WindowProc function. 

        let IME_COMPOSITIONFULL = 0x0284

        /// Sent to an application when the operating system is about to change the current IME. A window receives this message through its WindowProc function. 

        let IME_SELECT = 0x0285

        /// Sent to an application when the IME gets a character of the conversion result. A window receives this message through its WindowProc function. 

        let IME_CHAR = 0x0286

        /// Sent to an application to provide commands and request information. A window receives this message through its WindowProc function. 

        let IME_REQUEST = 0x0288

        /// Sent to an application by the IME to notify the application of a key press and to keep message order. A window receives this message through its WindowProc function. 

        let IME_KEYDOWN = 0x0290

        /// Sent to an application by the IME to notify the application of a key release and to keep message order. A window receives this message through its WindowProc function. 

        let IME_KEYUP = 0x0291

        /// The WM_MOUSEHOVER message is posted to a window when the cursor hovers over the client area of the window for the period of time specified in a prior call to TrackMouseEvent.

        let MOUSEHOVER = 0x02A1

        /// The WM_MOUSELEAVE message is posted to a window when the cursor leaves the client area of the window specified in a prior call to TrackMouseEvent.

        let MOUSELEAVE = 0x02A3

        /// The WM_NCMOUSEHOVER message is posted to a window when the cursor hovers over the nonclient area of the window for the period of time specified in a prior call to TrackMouseEvent.

        let NCMOUSEHOVER = 0x02A0

        /// The WM_NCMOUSELEAVE message is posted to a window when the cursor leaves the nonclient area of the window specified in a prior call to TrackMouseEvent.

        let NCMOUSELEAVE = 0x02A2

        /// The WM_WTSSESSION_CHANGE message notifies applications of changes in session state.

        let WTSSESSION_CHANGE = 0x02B1
        let TABLET_FIRST = 0x02c0
        let TABLET_LAST = 0x02df

        /// An application sends a WM_CUT message to an edit control or combo box to delete (cut) the current selection, if any, in the edit control and copy the deleted text to the clipboard in CF_TEXT format. 

        let CUT = 0x0300

        /// An application sends the WM_COPY message to an edit control or combo box to copy the current selection to the clipboard in CF_TEXT format. 

        let COPY = 0x0301

        /// An application sends a WM_PASTE message to an edit control or combo box to copy the current content of the clipboard to the edit control at the current caret position. Data is inserted only if the clipboard contains data in CF_TEXT format. 

        let PASTE = 0x0302

        /// An application sends a WM_CLEAR message to an edit control or combo box to delete (clear) the current selection, if any, from the edit control. 

        let CLEAR = 0x0303

        /// An application sends a WM_UNDO message to an edit control to undo the last operation. When this message is sent to an edit control, the previously deleted text is restored or the previously added text is deleted.

        let UNDO = 0x0304

        /// The WM_RENDERFORMAT message is sent to the clipboard owner if it has delayed rendering a specific clipboard format and if an application has requested data in that format. The clipboard owner must render data in the specified format and place it on the clipboard by calling the SetClipboardData function. 

        let RENDERFORMAT = 0x0305

        /// The WM_RENDERALLFORMATS message is sent to the clipboard owner before it is destroyed, if the clipboard owner has delayed rendering one or more clipboard formats. For the content of the clipboard to remain available to other applications, the clipboard owner must render data in all the formats it is capable of generating, and place the data on the clipboard by calling the SetClipboardData function. 

        let RENDERALLFORMATS = 0x0306

        /// The WM_DESTROYCLIPBOARD message is sent to the clipboard owner when a call to the EmptyClipboard function empties the clipboard. 

        let DESTROYCLIPBOARD = 0x0307

        /// The WM_DRAWCLIPBOARD message is sent to the first window in the clipboard viewer chain when the content of the clipboard changes. This enables a clipboard viewer window to display the new content of the clipboard. 

        let DRAWCLIPBOARD = 0x0308

        /// The WM_PAINTCLIPBOARD message is sent to the clipboard owner by a clipboard viewer window when the clipboard contains data in the CF_OWNERDISPLAY format and the clipboard viewer's client area needs repainting. 

        let PAINTCLIPBOARD = 0x0309

        /// The WM_VSCROLLCLIPBOARD message is sent to the clipboard owner by a clipboard viewer window when the clipboard contains data in the CF_OWNERDISPLAY format and an event occurs in the clipboard viewer's vertical scroll bar. The owner should scroll the clipboard image and update the scroll bar values. 

        let VSCROLLCLIPBOARD = 0x030A

        /// The WM_SIZECLIPBOARD message is sent to the clipboard owner by a clipboard viewer window when the clipboard contains data in the CF_OWNERDISPLAY format and the clipboard viewer's client area has changed size. 

        let SIZECLIPBOARD = 0x030B

        /// The WM_ASKCBFORMATNAME message is sent to the clipboard owner by a clipboard viewer window to request the name of a CF_OWNERDISPLAY clipboard format.

        let ASKCBFORMATNAME = 0x030C

        /// The WM_CHANGECBCHAIN message is sent to the first window in the clipboard viewer chain when a window is being removed from the chain. 

        let CHANGECBCHAIN = 0x030D

        /// The WM_HSCROLLCLIPBOARD message is sent to the clipboard owner by a clipboard viewer window. This occurs when the clipboard contains data in the CF_OWNERDISPLAY format and an event occurs in the clipboard viewer's horizontal scroll bar. The owner should scroll the clipboard image and update the scroll bar values. 

        let HSCROLLCLIPBOARD = 0x030E

        /// This message informs a window that it is about to receive the keyboard focus, giving the window the opportunity to realize its logical palette when it receives the focus. 

        let QUERYNEWPALETTE = 0x030F

        /// The WM_PALETTEISCHANGING message informs applications that an application is going to realize its logical palette. 

        let PALETTEISCHANGING = 0x0310

        /// This message is sent by the OS to all top-level and overlapped windows after the window with the keyboard focus realizes its logical palette. 
        /// This message enables windows that do not have the keyboard focus to realize their logical palettes and update their client areas.

        let PALETTECHANGED = 0x0311

        /// The WM_HOTKEY message is posted when the user presses a hot key registered by the RegisterHotKey function. The message is placed at the top of the message queue associated with the thread that registered the hot key. 

        let HOTKEY = 0x0312

        /// The WM_PRINT message is sent to a window to request that it draw itself in the specified device context, most commonly in a printer device context.

        let PRINT = 0x0317

        /// The WM_PRINTCLIENT message is sent to a window to request that it draw its client area in the specified device context, most commonly in a printer device context.

        let PRINTCLIENT = 0x0318

        /// The WM_APPCOMMAND message notifies a window that the user generated an application command event, for example, by clicking an application command button using the mouse or typing an application command key on the keyboard.

        let APPCOMMAND = 0x0319

        /// The WM_THEMECHANGED message is broadcast to every window following a theme change event. Examples of theme change events are the activation of a theme, the deactivation of a theme, or a transition from one theme to another.

        let THEMECHANGED = 0x031A

        /// Sent when the contents of the clipboard have changed.

        let CLIPBOARDUPDATE = 0x031D

        /// The system will send a window the WM_DWMCOMPOSITIONCHANGED message to indicate that the availability of desktop composition has changed.

        let DWMCOMPOSITIONCHANGED = 0x031E

        /// WM_DWMNCRENDERINGCHANGED is called when the non-client area rendering status of a window has changed. Only windows that have set the flag DWM_BLURBEHIND.fTransitionOnMaximized to true will get this message. 

        let DWMNCRENDERINGCHANGED = 0x031F

        /// Sent to all top-level windows when the colorization color has changed. 

        let DWMCOLORIZATIONCOLORCHANGED = 0x0320

        /// WM_DWMWINDOWMAXIMIZEDCHANGE will let you know when a DWM composed window is maximized. You also have to register for this message as well. You'd have other windowd go opaque when this message is sent.

        let DWMWINDOWMAXIMIZEDCHANGE = 0x0321

        /// Sent to request extended title bar information. A window receives this message through its WindowProc function.

        let GETTITLEBARINFOEX = 0x033F
        let HANDHELDFIRST = 0x0358
        let HANDHELDLAST = 0x035F
        let AFXFIRST = 0x0360
        let AFXLAST = 0x037F
        let PENWINFIRST = 0x0380
        let PENWINLAST = 0x038F

        /// The WM_APP constant is used by applications to help define private messages, usually of the form WM_APP+X, where X is an integer value. 

        let APP = 0x8000

        /// The WM_USER constant is used by applications to help define private messages for use by private window classes, usually of the form WM_USER+X, where X is an integer value. 

        let USER = 0x0400
        
        
        /// An application sends the WM_CPL_LAUNCH message to Windows Control Panel to request that a Control Panel application be started. 

        let CPL_LAUNCH = USER+0x1000
        
        /// The WM_CPL_LAUNCHED message is sent when a Control Panel application, started by the WM_CPL_LAUNCH message, has closed. The WM_CPL_LAUNCHED message is sent to the window identified by the wParam parameter of the WM_CPL_LAUNCH message that started the application. 

        let CPL_LAUNCHED = USER+0x1001
        
        /// WM_SYSTIMER is a well-known yet still undocumented message. Windows uses WM_SYSTIMER for internal actions like scrolling.

        let SYSTIMER = 0x118
        
        
        /// The accessibility state has changed.

        let HSHELL_ACCESSIBILITYSTATE = 11
        
        /// The shell should activate its main window.

        let HSHELL_ACTIVATESHELLWINDOW = 3
        
        /// The user completed an input event (for example, pressed an application command button on the mouse or an application command key on the keyboard), and the application did not handle the WM_APPCOMMAND message generated by that input.
        /// If the Shell procedure handles the WM_COMMAND message, it should not call CallNextHookEx. See the Return Value section for more information.

        let HSHELL_APPCOMMAND = 12
        
        /// A window is being minimized or maximized. The system needs the coordinates of the minimized rectangle for the window.

        let HSHELL_GETMINRECT = 5
        
        /// Keyboard language was changed or a new keyboard layout was loaded.

        let HSHELL_LANGUAGE = 8
        
        /// The title of a window in the task bar has been redrawn.

        let HSHELL_REDRAW = 6
        
        /// The user has selected the task list. A shell application that provides a task list should return TRUE to prevent Windows from starting its task list.

        let HSHELL_TASKMAN = 7
        
        /// A top-level, unowned window has been created. The window exists when the system calls this hook.

        let HSHELL_WINDOWCREATED = 1
        
        /// A top-level, unowned window is about to be destroyed. The window still exists when the system calls this hook.

        let HSHELL_WINDOWDESTROYED = 2
        
        /// The activation has changed to a different top-level, unowned window.

        let HSHELL_WINDOWACTIVATED = 4
        
        /// A top-level window is being replaced. The window exists when the system calls this hook.

        let HSHELL_WINDOWREPLACED = 13
        
        ()
  
  


    module Constants =
        /// 2
        let WM_DESTROY = 0x0002
        /// 12
        let WM_QUIT = 0x0012
        /// 16
        let WM_CLOSE = 16 // &H10
        let WM_SYSCOMMAND = 274 //&H112
        let WM_COMMAND = 273 // &H111
        let BM_CLICK = 0x00F5 // 245, &HF5 // simulates button click sending a WM Down and WM  UP
        let WM_LBUTTONDOWN = 0x0201
        let WM_LBUTTONUP = 0x0202
    
    open Constants
//    Function MakeDWord(ByVal LoWord As Integer, ByVal HiWord As Integer) As Long
//        MakeDWord = (HiWord * &H10000) Or (LoWord And &HFFFF&)
//    End Function
    let makeDWord loWord hiWord =
        hiWord <<< 16 ||| (loWord &&& 65535)
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms645608(v=vs.85).aspx
    type WParamModifiers =
        |Ctrl
        |MMButton
        |RMButton
        |Shift
        |MXButton
        |MXButton2
        
    type Message = 
        /// This message causes the button to receive the WM_LBUTTONDOWN and WM_LBUTTONUP messages, and the button's parent window to receive a BN_CLICKED notification code.    
        |BmClick // does not allow params
        |LButtonDown of Point*WParamModifiers list
        |LButtonUp of Point*WParamModifiers list
    let windowFromPoint pt =
        WindowFromPoint(pt)
    let sendMessage' hWnd msg wParam lParam =
        SendMessage(hWnd, msg, wParam, lParam)
        
    let sendMessage h =
        function
        | BmClick ->
            SendMessage(h, BM_CLICK, IntPtr.Zero, IntPtr.Zero)
        | LButtonDown (pt,wParams) ->
            // relative to the window it seems per http://www.jasinskionline.com/windowsapi/ref/w/wm_lbuttondown.html
            /// The low-order word specifies the x-coordinate of the cursor. The coordinate is relative to the upper-left corner of the client area.
            /// The high-order word specifies the y-coordinate of the cursor. The coordinate is relative to the upper-left corner of the client area.
            let lParam = makeDWord pt.X pt.Y
            
            SendMessage(h, BM_CLICK, IntPtr.Zero, lParam)
        | LButtonUp (pt,wParams) ->
            ()
let loopSendMessageValues () =
    // iterate messages, searching for something useful
    let LeClick pt =
        // Get a handle
        let handle = PInvoke.windowFromPoint pt
        let closers = [
                PInvoke.Constants.WM_DESTROY
                PInvoke.Constants.WM_QUIT
                PInvoke.Constants.WM_CLOSE
            ]
        // Send the click message
        if handle <> IntPtr.Zero then
            for i in [0..0x00F8] do
            
                if closers |> Seq.exists ((=) i) |> not then 
                    Console.WriteLine("Sending a " + string i)
                    PInvoke.sendMessage' handle i IntPtr.Zero IntPtr.Zero |> ignore<IntPtr>
                    System.Threading.Thread.Sleep(1500)
        else Console.WriteLine("Failed to get window from point")
    
    
    
    let positionOfInterest = Util.Cache(fun () ->
        Util.ReadLine("Put mouse over click target") |> ignore<string>
        System.Windows.Forms.Cursor.Position
    )
    System.Threading.Thread.Sleep(2000)
    positionOfInterest.Dump("Clicking at")
    LeClick(positionOfInterest)



// succeeded in sending a 2 message at least, whatever that is
//
//void Main()
//{
//    var positionOfInterest = Util.Cache(() =>
//    {
//        Util.ReadLine("Put mouse over click target");
//        return Cursor.Position;
//    });
//    System.Threading.Thread.Sleep(2000);
//    positionOfInterest.Dump("Clicking at");
//    LeClick(positionOfInterest);
//
//}
//
//// Define other methods and classes here
//[DllImport("user32.dll")]
//private static extern IntPtr SendMessage(IntPtr hWnd, int Msg,
//       IntPtr wParam, IntPtr lParam);
//
//[DllImport("user32.dll", EntryPoint = "WindowFromPoint",
//    CharSet = CharSet.Auto, ExactSpelling = true)]
//public static extern IntPtr WindowFromPoint(Point point);
///// This message causes the button to receive the WM_LBUTTONDOWN and WM_LBUTTONUP messages, and the button's parent window to receive a BN_CLICKED notification code.
//const int BM_CLICK = 0x00F5; // simulates button click sending a WM Down and WM  UP
//const int WM_LBUTTONDOWN = 0x0201;
//const int WM_LBUTTONUP = 0x0202;
//const int Close = 0x0002;
//
//
//void LeClick(Point point)
//{
//    // Get a handle
//    var handle = WindowFromPoint(point);
//    // Send the click message
//    if (handle != IntPtr.Zero)
//    {
//        for(var i = 0x0000; i < 0x00F8; i++){
//            Console.WriteLine("Sending a " + i);
//            SendMessage(handle, i, IntPtr.Zero, IntPtr.Zero);	
//            System.Threading.Thread.Sleep(1500);
//        }
//        
//    }
//    else Console.WriteLine("Failed to get window from point");
//}
//
//