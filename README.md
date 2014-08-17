# About

This is a really simple system for handling queued emails. It reads
emails out of a database table, and tries to send a specified number
every second (well, it delays a second between each sending, so it is
upper bounded by the specified number per second, but probably will be
less, accounting for latency, request time, etc). It records when a
message was sent. It uses the database to lock messages, so many workers
can work simultaneously (but they will all try to send out the configured
number of messages per second).

If messages fail to send for any reason (this could be connectivity,
blacklisted addresses, etc, but NOT bounced messages, as those are
sent, but come back later), the message has its failed_count
incremented, and when that hits 3, we stop trying to send it. Messages
are sent in plain text unless html is true, in which case they are
sent as html. Right now there is no way to send both.

# Software

This is designed to work with Amazon's Simple Email Service (SES), and
PostgreSQL. It wouldn't be too hard to adapt it to other backends, but
I wanted to keep it extremely simple, and everything I do is with
PostgreSQL.

# Setup

To use this, you need to create a table amazon_email_queue with the
following colums:

    id serial PRIMARY KEY,
    to_addr text NOT NULL,
    to_name text,
    from_addr text NOT NULL,
    from_name text NOT NULL,
    subject text NOT NULL,
    body text NOT NULL,
    date timestamptz NOT NULL DEFAULT now(),
    sent_at timestamptz,
    failed_count integer NOT NULL DEFAULT 0,
    html boolean NOT NULL DEFAULT false,
    processing boolean NOT NULL DEFAULT false

And you should put an index on sent_at, date, failed_count, and processing (used to filter the messages). A file `create.sql` is provided that does this.

Then you should create a config file, which uses the configurator package, and should look like:

    host = "host for postgresql"

    port = 5433

    user = "user for postgresql"

    pass = "password for postgresql"

    db = "database name"

    ses-access-key = "AWS access key"

    ses-secret-key = "AWS secret key"

    limit = 5 -- or higher, if you have a higher rate limit

To start the mailer, pass it the path to this config file. (Note: configurator supports including other config files, and I chose the config names to overlap with the similar config file for the postgres snaplet, if you're using snap. In that case `import "path/to/prod.cfg"` will get you all the postgres settings).

Finally, in your application, just insert messages into the
`amazon_email_queue` table, and then should get picked up and sent out.


# Queue Cleaner

If you send a bunch of emails, eventually the queue will slow down,
because even with indexes the operation to get a message from the
queue slows down (if someone who knows postgres better than me knows a
different sort of index that doesn't have this property, let me
know!). Because of this, it makes sense to move messages out of the
queue periodically. The included amazon-email-queue-cleaner executable
moves messages into an `amazon_email_archive` table which should have a schema
with a subset of the columns in the queue table, and the columns defined in
a comma separated list in the config file. It will copy those columns out.

    columns = "to_addr, to_name, from_addr, from_name, subject"

It is organized like this because it is often convenient to add
additional application specific data to the email rows, and that data
should be able to be copied to the archive. Further, there is
information that isn't really important (like processing,
failed_count) in the archive that is needed for the queue.

It should be run with the same parameters as the `amazon-emailer`
executable, and will move, every minute, all sent messages to the
archive table.
