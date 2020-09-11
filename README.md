# cl-vote
### _inaimathi <leo.zovic@google.com>_

This is a tool for collective decision making.

#### General TODO

- Add hammer protection to the login system
	- Track failed login attempts by username
	- Clear them on success, or after some amount of time without activity
	- The login handler should wait to return some amount of time based on hammer number
	- After 25(more? less?) attempts, stop attempting to check login for that name from that device for some amount of time

- Allow actual voting

- Add ranking ballot

- Add result computation to elections

- Add admin panel for elections
	- Add/remove candidates (and possibly recompute points allowed)
	- Change decision date
	- Close election
	- Change ballot type?

- Make it at least _a little_ pretty

- List public elections

- Make the model actually write a database to disk

- Add option to allow voters to nominate candidates

#### Ways to vary elections
- ballot types
	- star votes (0 to 5)
	- good/bad ratio (-5 to 5)
	- up/down (-1, 0 or 1)
	- y/n (0 or 1)
	- point-distribution (number of points) (give points back when candidates are removed)
	=> (:range Min Max) (:points Int)

- candidates
	- fixed list
	- flexible list
	- voter-nominated (voters can add new candidates themselves; possibly a limit)

- decision time
	- future date
	- number of votes
	- close by admin
	- always open
	- all voters voted
	=> (:date Date) (:vote-count Int) (:all-voted) (:open)

- voters
	- fixed list
	- anyone

## License

Specify license here
