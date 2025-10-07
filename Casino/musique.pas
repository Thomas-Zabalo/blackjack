AUDIO_FREQUENCY :INTEGER=22050; AUDIO_FORMAT : WORD = AUDIO_S16 ;
AUDIO_CHANNELS :INTEGER=2; AUDIO_CHUNKSIZE :INTEGER=4096;

procedure son ( var sound : pMIX_MUSIC );
begin
	if MIX_OpenAudio ( AUDIO_FREQUENCY , AUDIO_FORMAT , AUDIO_CHANNELS ,
	AUDIO_CHUNKSIZE ) < >0 then HALT ;
		sound := MIX_LOADMUS ( ’ ressources / Irish_tavern_music.wav ’ );
	MIX_VolumeMusic ( MIX_MAX_VOLUME );
	MIX_PlayMusic ( sound , -1);
end ;
procedure termine_musique ( var sound : pMIX_MUSIC );
begin
	MIX_FREEMUSIC ( sound );
	Mix_CloseAudio ();
end ;
