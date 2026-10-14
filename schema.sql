-- Інформаційна система «Розклад роботи дисплейних класів факультету»
-- mysql -u root -p < schema.sql
--
-- Таблиці:
--   classrooms   — дисплейні класи
--   workstations — окремі робочі місця класів
--   users        — користувачі системи
--   teachers     — дані викладачів (розширення users)
--   subjects     — дисципліни
--   schedule     — розклад планових занять
--   free_access  — час вільного доступу до класів
--   usage_log    — журнал використання робочих місць (джерело даних про завантаженість)

SET NAMES utf8mb4;

CREATE DATABASE IF NOT EXISTS display_classes
  CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE display_classes;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS usage_log, free_access, schedule, subjects, teachers, users, workstations, classrooms;
SET FOREIGN_KEY_CHECKS = 1;

CREATE TABLE classrooms (
  id        INT AUTO_INCREMENT PRIMARY KEY,
  name      VARCHAR(100) NOT NULL UNIQUE,
  building  VARCHAR(100) NOT NULL,
  capacity  INT NOT NULL,                      
  CONSTRAINT chk_classrooms_capacity CHECK (capacity > 0)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE workstations (
  id            INT AUTO_INCREMENT PRIMARY KEY,
  classroom_id  INT NOT NULL,
  inv_number    VARCHAR(50) NOT NULL UNIQUE,  
  specs         VARCHAR(255) NULL,        
  status        ENUM('WORKING','REPAIR','DISABLED') NOT NULL DEFAULT 'WORKING',
  CONSTRAINT fk_ws_classroom FOREIGN KEY (classroom_id) REFERENCES classrooms (id)
    ON DELETE RESTRICT ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE users (
  id         INT AUTO_INCREMENT PRIMARY KEY,
  login      VARCHAR(50)  NOT NULL UNIQUE,
  full_name  VARCHAR(150) NOT NULL,
  role       ENUM('ADMIN','TEACHER','STUDENT','TECHNICIAN') NOT NULL,
  email      VARCHAR(150) NULL,
  is_active  TINYINT(1)   NOT NULL DEFAULT 1
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE teachers (
  id          INT AUTO_INCREMENT PRIMARY KEY,
  user_id     INT NOT NULL UNIQUE,
  department  VARCHAR(150) NOT NULL,
  job_title   VARCHAR(100) NOT NULL,
  CONSTRAINT fk_teachers_user FOREIGN KEY (user_id) REFERENCES users (id)
    ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE subjects (
  id    INT AUTO_INCREMENT PRIMARY KEY,
  code  VARCHAR(20)  NOT NULL UNIQUE,
  name  VARCHAR(200) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE schedule (
  id            INT AUTO_INCREMENT PRIMARY KEY,
  classroom_id  INT NOT NULL,
  teacher_id    INT NOT NULL,
  subject_id    INT NOT NULL,
  group_name    VARCHAR(30) NOT NULL,
  week_day      ENUM('MON','TUE','WED','THU','FRI','SAT','SUN') NOT NULL,
  start_time    TIME NOT NULL,
  end_time      TIME NOT NULL,
  CONSTRAINT chk_schedule_time CHECK (end_time > start_time),
  CONSTRAINT fk_sched_classroom FOREIGN KEY (classroom_id) REFERENCES classrooms (id)
    ON DELETE RESTRICT ON UPDATE CASCADE,
  CONSTRAINT fk_sched_teacher FOREIGN KEY (teacher_id) REFERENCES teachers (id)
    ON DELETE RESTRICT ON UPDATE CASCADE,
  CONSTRAINT fk_sched_subject FOREIGN KEY (subject_id) REFERENCES subjects (id)
    ON DELETE RESTRICT ON UPDATE CASCADE,
  INDEX idx_sched_slot (classroom_id, week_day, start_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE free_access (
  id            INT AUTO_INCREMENT PRIMARY KEY,
  classroom_id  INT NOT NULL,
  week_day      ENUM('MON','TUE','WED','THU','FRI','SAT','SUN') NOT NULL,
  start_time    TIME NOT NULL,
  end_time      TIME NOT NULL,
  note          VARCHAR(200) NULL,
  CONSTRAINT chk_free_time CHECK (end_time > start_time),
  CONSTRAINT fk_free_classroom FOREIGN KEY (classroom_id) REFERENCES classrooms (id)
    ON DELETE RESTRICT ON UPDATE CASCADE,
  INDEX idx_free_slot (classroom_id, week_day, start_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE usage_log (
  id              INT AUTO_INCREMENT PRIMARY KEY,
  workstation_id  INT NOT NULL,
  user_id         INT NOT NULL,
  lesson_id       INT NULL,
  kind            ENUM('LESSON','FREE_ACCESS') NOT NULL,
  started_at      DATETIME NOT NULL,
  ended_at        DATETIME NULL,
  CONSTRAINT chk_usage_time CHECK (ended_at IS NULL OR ended_at >= started_at),
  CONSTRAINT fk_usage_ws FOREIGN KEY (workstation_id) REFERENCES workstations (id)
    ON DELETE RESTRICT ON UPDATE CASCADE,
  CONSTRAINT fk_usage_user FOREIGN KEY (user_id) REFERENCES users (id)
    ON DELETE RESTRICT ON UPDATE CASCADE,
  CONSTRAINT fk_usage_lesson FOREIGN KEY (lesson_id) REFERENCES schedule (id)
    ON DELETE SET NULL ON UPDATE CASCADE,
  INDEX idx_usage_ws_time (workstation_id, started_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- --------------------------------------------------------------------------------
-- Тестові дані
-- --------------------------------------------------------------------------------

INSERT INTO classrooms (id, name, building, capacity) VALUES
  (1, 'Дисплейний клас 301', 'Корпус 1, ауд. 301', 8),
  (2, 'Дисплейний клас 305', 'Корпус 1, ауд. 305', 6),
  (3, 'Дисплейний клас 412', 'Корпус 2, ауд. 412', 6);

INSERT INTO workstations (id, classroom_id, inv_number, specs, status) VALUES
  ( 1, 1, 'PC-301-01', 'Core i5-12400, 16 GB RAM, 512 GB SSD', 'WORKING'),
  ( 2, 1, 'PC-301-02', 'Core i5-12400, 16 GB RAM, 512 GB SSD', 'WORKING'),
  ( 3, 1, 'PC-301-03', 'Core i5-12400, 16 GB RAM, 512 GB SSD', 'WORKING'),
  ( 4, 1, 'PC-301-04', 'Core i5-12400, 16 GB RAM, 512 GB SSD', 'WORKING'),
  ( 5, 1, 'PC-301-05', 'Core i5-12400, 16 GB RAM, 512 GB SSD', 'REPAIR'),
  ( 6, 1, 'PC-301-06', 'Core i5-12400, 16 GB RAM, 512 GB SSD', 'WORKING'),
  ( 7, 2, 'PC-305-01', 'Ryzen 5 5600, 16 GB RAM, 512 GB SSD',  'WORKING'),
  ( 8, 2, 'PC-305-02', 'Ryzen 5 5600, 16 GB RAM, 512 GB SSD',  'WORKING'),
  ( 9, 2, 'PC-305-03', 'Ryzen 5 5600, 16 GB RAM, 512 GB SSD',  'WORKING'),
  (10, 2, 'PC-305-04', 'Ryzen 5 5600, 16 GB RAM, 512 GB SSD',  'WORKING'),
  (11, 2, 'PC-305-05', 'Ryzen 5 5600, 16 GB RAM, 512 GB SSD',  'DISABLED'),
  (12, 3, 'PC-412-01', 'Core i7-13700, 32 GB RAM, 1 TB SSD',   'WORKING'),
  (13, 3, 'PC-412-02', 'Core i7-13700, 32 GB RAM, 1 TB SSD',   'WORKING'),
  (14, 3, 'PC-412-03', 'Core i7-13700, 32 GB RAM, 1 TB SSD',   'WORKING'),
  (15, 3, 'PC-412-04', 'Core i7-13700, 32 GB RAM, 1 TB SSD',   'WORKING');

INSERT INTO users (id, login, full_name, role, email, is_active) VALUES
  (1, 'admin',     'Адміністратор системи',        'ADMIN',      'admin@faculty.example',     1),
  (2, 'petrenko',  'Петренко Олена Іванівна',      'TEACHER',    'petrenko@faculty.example',  1),
  (3, 'koval',     'Коваль Андрій Михайлович',     'TEACHER',    'koval@faculty.example',     1),
  (4, 'shevchuk',  'Шевчук Наталія Петрівна',      'TEACHER',    'shevchuk@faculty.example',  1),
  (5, 'bondar',    'Бондар Ігор Васильович',       'TECHNICIAN', NULL,                        1),
  (6, 'melnyk',    'Мельник Богдан Олегович',      'STUDENT',    'melnyk@student.example',    1),
  (7, 'ivanenko',  'Іваненко Софія Андріївна',     'STUDENT',    'ivanenko@student.example',  1),
  (8, 'tkachenko', 'Ткаченко Максим Сергійович',   'STUDENT',    NULL,                        1),
  (9, 'kravchenko','Кравченко Аліна Вікторівна',   'STUDENT',    'kravchenko@student.example',1);

INSERT INTO teachers (id, user_id, department, job_title) VALUES
  (1, 2, 'Кафедра програмної інженерії', 'доцент'),
  (2, 3, 'Кафедра комп''ютерних наук',   'старший викладач'),
  (3, 4, 'Кафедра прикладної математики', 'професор');

INSERT INTO subjects (id, code, name) VALUES
  (1, 'PRG',  'Основи програмування'),
  (2, 'ALG',  'Алгоритми та структури даних'),
  (3, 'DB',   'Бази даних'),
  (4, 'FP',   'Функціональне програмування'),
  (5, 'NET',  'Комп''ютерні мережі');

INSERT INTO schedule (id, classroom_id, teacher_id, subject_id, group_name, week_day, start_time, end_time) VALUES
  ( 1, 1, 1, 4, 'КН-31', 'MON', '08:30', '09:50'),
  ( 2, 1, 2, 3, 'КН-32', 'MON', '10:10', '11:30'),
  ( 3, 1, 2, 2, 'КН-31', 'TUE', '08:30', '09:50'),
  ( 4, 2, 3, 1, 'КН-11', 'TUE', '10:10', '11:30'),
  ( 5, 2, 3, 5, 'КН-41', 'WED', '11:50', '13:10'),
  ( 6, 3, 1, 4, 'КН-32', 'THU', '08:30', '09:50'),
  ( 7, 3, 2, 3, 'КН-41', 'THU', '10:10', '11:30'),
  ( 8, 1, 3, 1, 'КН-12', 'FRI', '10:10', '11:30'),
  ( 9, 2, 1, 4, 'КН-33', 'MON', '13:30', '14:50');

INSERT INTO free_access (id, classroom_id, week_day, start_time, end_time, note) VALUES
  (1, 1, 'MON', '15:00', '18:00', 'Самостійна робота студентів'),
  (2, 1, 'WED', '15:00', '18:00', 'Самостійна робота студентів'),
  (3, 1, 'FRI', '14:00', '17:00', NULL),
  (4, 1, 'SAT', '09:00', '14:00', 'Підготовка до олімпіад'),
  (5, 2, 'TUE', '14:00', '17:00', NULL),
  (6, 2, 'THU', '14:00', '17:00', NULL),
  (7, 3, 'TUE', '15:00', '18:00', 'Курсові та дипломні роботи'),
  (8, 3, 'THU', '15:00', '18:00', 'Курсові та дипломні роботи');

INSERT INTO usage_log (workstation_id, user_id, lesson_id, kind, started_at, ended_at) VALUES
  ( 1, 6, 1, 'LESSON',      TIMESTAMP(CURDATE() - INTERVAL 5 DAY, '08:30:00'), TIMESTAMP(CURDATE() - INTERVAL 5 DAY, '09:50:00')),
  ( 2, 7, 1, 'LESSON',      TIMESTAMP(CURDATE() - INTERVAL 5 DAY, '08:32:00'), TIMESTAMP(CURDATE() - INTERVAL 5 DAY, '09:48:00')),
  ( 3, 8, 1, 'LESSON',      TIMESTAMP(CURDATE() - INTERVAL 5 DAY, '08:35:00'), TIMESTAMP(CURDATE() - INTERVAL 5 DAY, '09:50:00')),
  ( 1, 9, NULL, 'FREE_ACCESS', TIMESTAMP(CURDATE() - INTERVAL 4 DAY, '15:10:00'), TIMESTAMP(CURDATE() - INTERVAL 4 DAY, '17:20:00')),
  ( 2, 6, NULL, 'FREE_ACCESS', TIMESTAMP(CURDATE() - INTERVAL 4 DAY, '15:30:00'), TIMESTAMP(CURDATE() - INTERVAL 4 DAY, '16:45:00')),
  ( 7, 7, 4, 'LESSON',      TIMESTAMP(CURDATE() - INTERVAL 3 DAY, '10:10:00'), TIMESTAMP(CURDATE() - INTERVAL 3 DAY, '11:30:00')),
  ( 8, 8, 4, 'LESSON',      TIMESTAMP(CURDATE() - INTERVAL 3 DAY, '10:12:00'), TIMESTAMP(CURDATE() - INTERVAL 3 DAY, '11:28:00')),
  ( 9, 9, NULL, 'FREE_ACCESS', TIMESTAMP(CURDATE() - INTERVAL 2 DAY, '14:05:00'), TIMESTAMP(CURDATE() - INTERVAL 2 DAY, '16:00:00')),
  (12, 6, 6, 'LESSON',      TIMESTAMP(CURDATE() - INTERVAL 2 DAY, '08:30:00'), TIMESTAMP(CURDATE() - INTERVAL 2 DAY, '09:50:00')),
  (13, 7, 6, 'LESSON',      TIMESTAMP(CURDATE() - INTERVAL 2 DAY, '08:31:00'), TIMESTAMP(CURDATE() - INTERVAL 2 DAY, '09:49:00')),
  (12, 8, NULL, 'FREE_ACCESS', TIMESTAMP(CURDATE() - INTERVAL 1 DAY, '15:15:00'), TIMESTAMP(CURDATE() - INTERVAL 1 DAY, '17:45:00')),
  ( 3, 9, NULL, 'FREE_ACCESS', TIMESTAMP(CURDATE() - INTERVAL 1 DAY, '15:00:00'), TIMESTAMP(CURDATE() - INTERVAL 1 DAY, '16:10:00')),
  ( 1, 7, 8, 'LESSON',      TIMESTAMP(CURDATE() - INTERVAL 8 DAY, '10:10:00'), TIMESTAMP(CURDATE() - INTERVAL 8 DAY, '11:30:00')),
  ( 4, 8, 8, 'LESSON',      TIMESTAMP(CURDATE() - INTERVAL 8 DAY, '10:12:00'), TIMESTAMP(CURDATE() - INTERVAL 8 DAY, '11:25:00')),
  ( 2, 6, NULL, 'FREE_ACCESS', NOW() - INTERVAL 25 MINUTE, NULL);