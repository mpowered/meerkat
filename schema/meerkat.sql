CREATE EXTENSION IF NOT EXISTS timescaledb;

CREATE TABLE action_controller (
    "time" timestamp with time zone NOT NULL,
    host text NOT NULL,
    app text NOT NULL,
    user_id text,
    account_id text,
    scorecard_id text,
    controller text NOT NULL,
    action text NOT NULL,
    params jsonb NOT NULL,
    format text NOT NULL,
    method text NOT NULL,
    path text NOT NULL,
    status text NOT NULL,
    view_runtime double precision,
    db_runtime double precision,
    total_runtime double precision
);

SELECT create_hypertable('action_controller', 'time');

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
    queue text NOT NULL,
    length bigint
);

SELECT create_hypertable('sidekiq_queues', 'time');
