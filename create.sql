CREATE TABLE amazon_email_queue (
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
);

CREATE INDEX ON amazon_email_queue (sent_at);
CREATE INDEX ON amazon_email_queue (date);
CREATE INDEX ON amazon_email_queue (failed_count);
CREATE INDEX ON amazon_email_queue (processing);
