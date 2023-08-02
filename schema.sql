CREATE TABLE messages (
       id INT,
       deltas TEXT,
       secs INT,
       deleted BOOLEAN NOT NULL DEFAULT FALSE
);
