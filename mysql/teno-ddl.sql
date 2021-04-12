DROP TABLE IF EXISTS memo_text;

DROP TABLE IF EXISTS memos;

CREATE TABLE memos (
  memo_id char(36) NOT NULL PRIMARY KEY,
  created_on datetime NOT NULL
) engine=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE memo_text (
  memo_id char(36) NOT NULL PRIMARY KEY,
  string text NOT NULL,
  FOREIGN KEY (memo_id) REFERENCES memos(memo_id)
) engine=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
