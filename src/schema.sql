create table run (
	id serial primary key,
	game text not null,
	fps double precision check (0 < fps and fps < 'infinity'),
	started timestamp with time zone not null
);

create table split (
	run integer references run,
	seq_no integer not null check (0 <= seq_no),
	state text not null check ('STOP' != state),
	/* 0 microseconds is the start of the run */
	microsecond bigint not null,
	frame integer,
	primary key (run, seq_no)
);

/* TODO: indexes */
