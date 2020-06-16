CREATE TABLE nodes
(
    id    SERIAL NOT NULL
        CONSTRAINT nodes_pk
            PRIMARY KEY,
    label TEXT   NOT NULL
);

CREATE TABLE links
(
    id_left  INTEGER NOT NULL,
    id_right INTEGER NOT NULL,
    CONSTRAINT links_pk
        PRIMARY KEY (id_right, id_left),
    CONSTRAINT links_check
        CHECK (id_left <> id_right)
);

CREATE UNIQUE INDEX links_greatest_least_idx
    ON links (GREATEST(id_left, id_right), LEAST(id_left, id_right));
