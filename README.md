# todoapp
experimenting with building command-line todo app on Haskell with SQL DB as backend


It works with external SQL DB. (that is how I plan to sync it across devices).


Connections parameters now hardcoded in todo.hs


to list entries:


  `./todo`


to add entry:


  `./todo a Description of task spaces are no problem`


to add subentry (subtask) put parent ID after `a`:


  `./todo a ParentID Description of subtask`


to del entries (5 and 6 in this example)


  `./todo d 5 6`


to schedule task 5 to day 25 Sep 1977 (assuming time from 00:00 till 24:00)


  `./todo s 5 25.09.1977`


to schedule task 5 to specific time day


  `./todo s 5 10:00 25.09.1977 - 15:00 26.09.1977



you can omit start or end time (keeping `-` for end-time although) to schedule
a task without start or end time.


you can omit year or month if you expect a current year or current month


you can use the shorter form if you schedule in your schedule time is one day


  `./todo s 5 10:00-18:00 25.09.1977`

Only numeric input of dates are supported.

That's all for now. The database scheme can be changed in future versions.


