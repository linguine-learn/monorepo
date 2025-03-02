-- migrate:up
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    email TEXT UNIQUE NOT NULL,
    password TEXT,
    created_at TIMESTAMPTZ  default now()
);

CREATE TABLE user_session (
    id TEXT NOT NULL PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES users(id),
    expires_at TIMESTAMPTZ NOT NULL
);

-- migrate:down
DROP TABLE user_session;
DROP TABLE users;
