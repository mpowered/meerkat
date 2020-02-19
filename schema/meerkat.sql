CREATE EXTENSION IF NOT EXISTS timescaledb;

CREATE TABLE disk_space_usage (
    "time" timestamp with time zone NOT NULL,
    host text NOT NULL,
    source text NOT NULL,
    target text NOT NULL,
    fstype text,
    size bigint,
    used bigint,
    avail bigint
);

SELECT create_hypertable('disk_space_usage', 'time');

CREATE TABLE memory_usage (
    "time" timestamp with time zone NOT NULL,
    host text NOT NULL,
    total_mem bigint,
    used_mem bigint,
    free_mem bigint,
    shared_mem bigint,
    buffer_mem bigint,
    cache_mem bigint,
    avail_mem bigint,
    total_swap bigint,
    used_swap bigint,
    free_swap bigint
);

SELECT create_hypertable('memory_usage', 'time');

CREATE TABLE process_stats (
    "time" timestamp with time zone NOT NULL,
    host text NOT NULL,
    command text NOT NULL,
    cpu double precision,
    user_cpu double precision,
    sys_cpu double precision,
    guest_cpu double precision,
    wait_cpu double precision,
    virtual_mem bigint,
    resident_mem bigint,
    mem double precision
);

SELECT create_hypertable('process_stats', 'time');

CREATE TABLE sidekiq_queues (
    "time" timestamp with time zone NOT NULL,
    "queue" text NOT NULL,
    "length" bigint
);

SELECT create_hypertable('sidekiq_queues', 'time');

CREATE TABLE sidekiq_jobs (
    job_id text PRIMARY KEY NOT NULL,
    "queue" text NOT NULL,
    class text NOT NULL,
    params jsonb NOT NULL,
    "enqueued_at" timestamp with time zone NOT NULL,
    "started_at" timestamp with time zone,
    "completed_at" timestamp with time zone
);

SELECT create_hypertable('sidekiq_jobs', 'enqueued_at');

CREATE TABLE puma (
    "time" timestamp with time zone NOT NULL,
    host text NOT NULL,
    worker integer NOT NULL,
    backlog integer,
    running integer,
    pool_capacity integer,
    max_threads integer
);

SELECT create_hypertable('puma', 'time');

CREATE TABLE honeybadger (
    "time" timestamp with time zone NOT NULL,
    environment text NOT NULL,
    faults bigint
);

SELECT create_hypertable('honeybadger', 'time');

CREATE TABLE action_controller (
    "time" timestamp with time zone NOT NULL,
    host text NOT NULL,
    app text NOT NULL,
    user_id text,
    account_id text,
    scorecard_id text,
    controller text NOT NULL,
    action text NOT NULL,
    params jsonb,
    format text,
    method text NOT NULL,
    path text NOT NULL,
    status text,
    view_runtime double precision,
    db_runtime double precision,
    total_runtime double precision
);

SELECT create_hypertable('action_controller', 'time');
