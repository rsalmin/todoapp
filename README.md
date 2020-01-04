# todoapp
experimenting with building command line todo app on haskell with SQL DB as backend

It's works with external SQL DB. (that is how I plan to sync it across devices).

Connections parameters now hardcoded in todo.hs

to list entries:

  `./todo`

to add entry:

  `./todo a Description of task spaces are not problem`

to add subentry (subtask) put parent ID after `a`:

  `./todo a ParentID Description of subtask`

to del entries (5 and 6 in this example)

  `./todo d 5 6`

That's all for now. Database scheme will be changed for sure.

