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

create view segment as (
	select run.game, run.fps, run.started, f.run, f.seq_no,
	       f.state as f_state, t.state as t_state,
	       t.microsecond - f.microsecond as duration,
	       f.microsecond as f_microsecond, t.microsecond as t_microsecond,
	       f.frame as f_frame, t.frame as t_frame
	from split as f
	join run on f.run = run.id
	join split t on f.run = t.run and f.seq_no + 1 = t.seq_no
);

/* TODO: indexes */
