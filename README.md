 # Xelmish.Forms

 Allows for the creation of forms and input fields to get user text input with in Xelmish and monogame. Some changes to the base Xelmish code was needed to accommodate the fields.

 This is code that is ready to be dropped in to a project as a nuget packaged and used, but maybe it can serve as a reference or source.

 ## What Exists

* A base Input field that contains code that is shared between other field types
* A float field that captures numeric input and has increment/decrement buttons
* A single line text field that captures text input
* A blinking cursor
* A windowed text box (though there are some issues still)

## What doesn't exist

* Selection/Highlighting of text
* Copy/Paste
* Controlling the cursor with the mouse
* The windowed text box follows the cursor and so doesn't stick to the end like a html text box would (basically moving the cursor left once it is outside of the length of the text box is broken atm)
