-- migrate:up
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) NOT NULL,
  username VARCHAR(25) NOT NULL,
  password text NOT NULL,
  refreshTokenversion INTEGER DEFAULT 1 NOT NULL
)

-- migrate:down
DROP TABLE users;
