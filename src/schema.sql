/* For vertumnion to read intervals correctly, you will need to configure
 * postgres to display them in ISO8601 format. Do this by running
 *
 *     show config_file;
 *
 * in a psql prompt, then modifying the intervalstyle setting to iso_8601.
 *
 * If changing this setting is problematic for you, contact me. It should be
 * possible to modify vertumnion to execute
 *
 *     set intervalstyle=iso_8601
 *
 * each time it creates a connection.
 */

create table run (
	id serial primary key,
	game text not null,
	fps double precision check (0 < fps and fps < 'infinity')
);

create table split (
	run integer references run,
	seq_no integer not null check (0 <= seq_no),
	state text not null check ('STOP' != state),
	moment timestamp with time zone not null,
	frame integer,
	primary key (run, seq_no)
);
