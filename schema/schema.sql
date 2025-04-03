CREATE TABLE person (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    surname TEXT NOT NULL,
    age INTEGER NOT NULL,
    car_id INTEGER NOT NULL,
    FOREIGN KEY (car_id) REFERENCES car (id)
);

CREATE TABLE car (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    brand TEXT NOT NULL,
    model TEXT NOT NULL,
    year INTEGER NOT NULL,
    mileage INTEGER NOT NULL,
    engine_id INTEGER NOT NULL,
    FOREIGN KEY (engine_id) REFERENCES engine (id)
);

CREATE TABLE engine (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    codename TEXT DEFAULT NULL,
    litre_capacity REAL NOT NULL,
    piston_count INTEGER NOT NULL,
    shape TEXT NOT NULL,
    horsepower INTEGER NOT NULL
)
