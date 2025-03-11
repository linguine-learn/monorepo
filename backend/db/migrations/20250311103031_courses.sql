-- migrate:up
CREATE TABLE courses (
    id SERIAL PRIMARY KEY,
    source_language VARCHAR(4) NOT NULL,
    target_language VARCHAR(4) NOT NULL,
    created_at TIMESTAMPTZ DEFAULT now(),
    uri TEXT NOT NULL
);

CREATE TABLE sections (
    id SERIAL PRIMARY KEY,
    created_at TIMESTAMPTZ DEFAULT now(),
    uri TEXT NOT NULL
);

CREATE TABLE units (
    id SERIAL PRIMARY KEY,
    created_at TIMESTAMPTZ DEFAULT now(),
    uri TEXT NOT NULL
);

-- migrate:down
DROP TABLE courses;
DROP TABLE sections;
DROP TABLE units;
