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
html boolean NOT NULL DEFAULT false
processing boolean NOT NULL DEFAULT false

And you should put an index on sent_at, date, failed_count, and processing (used to filter the messages).

Then you should create a Config.hs file, that looks like:

        {-# LANGUAGE OverloadedStrings #-}
    
    module Config where
    
    import Data.ByteString (ByteString)
    
    configHost :: String
    configHost = "host for postgresql"
    
    configUser :: String
    configUser = "user for postgresql"
    
    configPass :: String
    configPass = "password for postgresql"
    
    configDB :: String
    configDB = "database name"
    
    configAccessKey :: ByteString
    configAccessKey = "AWS access key"
    
    configSecretKey :: ByteString
    configSecretKey = "AWS secret key"
    
    configLimit :: Int
    configLimit = 5 -- or higher, if you have a higher rate limit

Finally, in your application, just insert messages into the
amazon_email_queue table, and then should get picked up and sent out.