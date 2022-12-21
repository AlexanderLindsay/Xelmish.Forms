 # Xelmish.Forms

 Allows for the creation of forms and input fields to get user text input with in Xelmish and monogame. Some changes to the base Xelmish code was needed to accommodate the fields.

 This is code that is ready to be dropped in to a project as a nuget packaged and used, but maybe it can serve as a reference or source.

 ## What Exists

* A base Input field that contains code that is shared between other field types
* A float field that captures numeric input and has increment/decrement buttons
* A single line text field that captures text input
* A blinking cursor
* A windowed text box
* changing focus by mouse or tab (tab only works once focus is put on a field to start with)

## What doesn't exist

* Selection/Highlighting of text
* Copy/Paste
* Controlling the cursor with the mouse
