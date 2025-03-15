-- migrate:up
CREATE TABLE courses (
    id TEXT PRIMARY KEY,
    source_language VARCHAR(4) NOT NULL,
    target_language VARCHAR(4) NOT NULL,
    created_at TIMESTAMPTZ DEFAULT now(),
    uri TEXT NOT NULL
);

CREATE TABLE sections (
    id SERIAL PRIMARY KEY,
    course_id TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT now(),
    uri TEXT NOT NULL,

    FOREIGN KEY(course_id) REFERENCES courses(id)
);

CREATE TABLE units (
    id SERIAL PRIMARY KEY,
    section_id INT NOT NULL,
    course_id TEXT NOT NULL,

    created_at TIMESTAMPTZ DEFAULT now(),
    uri TEXT NOT NULL,

    FOREIGN KEY(course_id) REFERENCES courses(id),
    FOREIGN KEY(section_id) REFERENCES sections(id)
);

-- migrate:down
DROP TABLE units;
DROP TABLE sections;
DROP TABLE courses;
