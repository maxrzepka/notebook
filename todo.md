* Menu to access notes by tags
  - href values correctly populated in nav list
  - add sub menu :
        define category ( tag most often used )
        sub menu contains any tags associated with this category
  - search notes by tag

* Authentification only for update and create :
  - display username when user logged in
  - add list of valid username in DB : users collection
  - oauth2 authentification or encrypted password in DB

* Memo workflow :
  - select tags to get the notes to be part of a memo session
  - tags like citation , qa are candidate for a memo session
  - view the pending items of a memo session
  - rate them
  - change a citation to a QA : find tags in text

* Local mode :
   - plain clj file as DB
   - sync  mongoDB with clj file and save the file as resource

* Handle error : no db  connection

* Edit existing note , link to create new note