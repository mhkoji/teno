DROP TABLE IF EXISTS note_text;

DROP TABLE IF EXISTS notes;

CREATE TABLE notes (
  note_id char(36) NOT NULL PRIMARY KEY,
  created_on datetime NOT NULL
) engine=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE note_text (
  note_id char(36) NOT NULL PRIMARY KEY,
  string text NOT NULL,
  FOREIGN KEY (note_id) REFERENCES notes(note_id)
) engine=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
