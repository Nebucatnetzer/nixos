{ custom, downloadDir, inputs, pkgs, ... }:
let
  podget = pkgs.writeScriptBin "podget"
    "${builtins.readFile (pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/dvehrs/podget/master/podget";
      sha256 = "sha256-b+rsRz37A8xndLIrQHqIwlNIUDwr7ZRTaclw8LnjbPk=";
    })
  }";
  podgetrc = ''
    # ----------------------------------------------------------------------------------------------------------------------------------
    # Podget configuration file created by version 0.9.0
    # [ NOTE:  Do not delete version line as it will be used by future versions to
    #          to test if configuration files have been updated with any required changes.
    # ----------------------------------------------------------------------------------------------------------------------------------
    # File name and location configuration:

    # Name of Server List configuration file
    CONFIG_SERVERLIST=serverlist

    # Directory to store session files
    # If this option is not configured then by default podget will place the session files in the directory defined by TMPDIR/podget or
    # if it is not defined in the users shell then the session files will be placed in the directory /tmp/podget.
    # If you prefer a different location, then configure this variable.
    # DIR_SESSION=/home/user/tmp/podget

    # Directory where to store downloaded files
    DIR_LIBRARY=${downloadDir}

    # Directory to store logs in
    # By default, logs are stored in DIR_LIBRARY/.LOG
    # If you prefer a different location, then configure this variable.
    # DIR_LOG=/home/andreas/POD/LOG

    # Set logging file names
    LOG_FAIL=errors
    LOG_COMPLETE=done

    # ----------------------------------------------------------------------------------------------------------------------------------
    # Download Options:

    # Wget base options
    # Commonly used options:
    #   -c                          Continue interupted downloads - While this flag is commonly used there are feeds that it can
    #                                   cause "403 Forbidden" errors.
    #   -nH                         No host directories (overrides .wgetrc defaults if necessary)
    #   --proxy=off                 To disable proxy set by environmental variable http_proxy
    #   --no-check-certificate      To disable HTTPS certificate checks.  Useful for sites that may be using self-signed cerficates
    #                                   and not those from a trusted service authority.
    #   --prefer-family=IPv4/IPv6   When DNS provides a choice of addresses to connect to a host, attempt to connect to the specified
    #                                   address family first.  If all addresses of the given family fail then the other family will be
    #                                   tried.  If set to 'none' then the addresses will be tried in the order provided by the server
    #                                   regardless of which family they are in (this is effectively the default option).
    #
    #                                   If you wish to force the use of IPv4 addresses only then you can use the "-4" or "--inet4-only"
    #                                   options.  Conversely, if you want to force the use of IPv6 addresses then you can set the "-6"
    #                                   or "--inet6-only" options.
    #   --content-disposition       [EXPERIMENTAL FEATURE] Wget will look for and use "Content-Disposition" headers received from the
    #                                   server.  This can result in extra round-trips to the server for a "HEAD" request.  This option
    #                                   is useful for servers that use the "Content-Disposition" header to hold the filename of the
    #                                   downloaded file rather than appending it to the URL.  This has the potential to make  some of
    #                                   Podget's FILENAME_FORMATFIX options unneeded.
    #
    #                                   WARNING:  Enabling this flag disables any download progress information from being passed on to
    #                                   the user.  To debug errors that may occur during sessions with this flag enabled, it may be
    #                                   necessary to enable DEBUG and then examine the temporary files that are not deleted in
    #                                   DIR_SESSION.
    #
    #                                   NOTE: This can be enable globally for all feeds here or if you want to enable it for only a few
    #                                   specific feeds, you can add "OPT_CONTENT_DISPOSITION" to their line in your serverlist.
    #
    # Wget options that include spaces need to be surrounded in quotes.
    #
    # WGET_BASEOPTS="-c --proxy=off --no-check-certificate"
    # WGET_BASEOPTS="-nH --proxy=off --content-disposition"
    # WGET_BASEOPTS="-c --prefer-family=IPv4"
    # WGET_BASEOPTS="-c --prefer-family=IPv6"
    WGET_BASEOPTS="-c -nH"

    # Most Recent
    # 0  == download all new items.
    # 1+ == download only the <count> most recent
    MOST_RECENT=10

    # Force
    # 0 == Only download new material.
    # 1 == Force download all items even those you've downloaded before.
    FORCE=0

    # Autocleanup of old playlists and the files they list.
    # 0 == disabled
    # 1 == delete any old content
    CLEANUP=0

    # Number of days to keep files.   Cleanup will remove anything
    # older than this.
    CLEANUP_DAYS=7

    # Stop downloading if available space on the partition drops below value (in KB)
    # default:  614400 (600MB)
    MIN_SPACE=614400

    # ----------------------------------------------------------------------------------------------------------------------------------
    # Playlist Options:

    # Disable playlist creation [ No need to comment out other playlist variables ]
    # 0 == create
    # 1 == do not create
    NO_PLAYLIST=1

    # Build playlists (comment out or set to a blank string to accept default format: New-).
    PLAYLIST_NAMEBASE=New-

    # Date format for new playlist names
    # +%F        = YYYY-MM-DD  like 2014-01-15  (DEFAULT)
    # +%m-%d-%Y  = MM-DD-YYYY  like 01-15-2014
    # For other options 'man date'
    #
    # Date options that include spaces need to be surrounded in quotes.
    #
    DATE_FORMAT=+%F

    # ASX Playlists for Windows Media Player
    # 0 == do not create
    # 1 == create
    ASX_PLAYLIST=0

    # ----------------------------------------------------------------------------------------------------------------------------------
    # Filename Suffix:

    # Add suffix to the filename of every file downloaded to allow for subsequent scripts to detect the newly downloaded files and work
    # on them.  Examples of this would be scripts to run id3v2 to force a standard genre for all MP3 files downloaded or to use mp3gain
    # to normalize files to have the same volume.
    #
    # A period (.) will automatically be added between the filename and tag like so:
    #       filename.mp3.newtag
    #
    # Tags will not be added to filenames as they are added to the playlists.  It will be necessary for any script that you run to
    # process the files remove the tag for the playlists to work.
    #
    # If this variable is undefined or commented out, then by default no suffix will be added.

    # FILENAME_SUFFIX="newtag"

    # ----------------------------------------------------------------------------------------------------------------------------------
    # Downloaded Filename Cleanup Options:
    #
    # These options are for the filenames downloaded from the feeds.  We will try to clean then up rather than interrupting the script
    # execution.

    # Filename Cleanup: For FAT32 filename compatability (Feature Request #1378956)
    # Tested with the following characters: !@#$%^&*()_-+=||\\{[\}]:;"'<,>.?/
    #
    # The \`, \" and \\ characters need to be escaped with a leading backslash.
    #
    # Bad Character definitions need to be surrounded in quotes.
    #
    # NOTE: FILENAME_BADCHARS is also used to test for characters that commonly cause errors in directory names.  This can cause
    # FILENAME_BADCHARS to be reported as part of an error for configuration issues with DIR_SESSION, DIR_LOG, DIR_LIBRARY and podcast
    # FEED_NAME and FEED_CATEGORY.
    FILENAME_BADCHARS="\`~!#$^&=+{}*[]:;\"'<>?|\\"

    # Filename Replace Character: Character to use to replace any/all
    # bad characters found.
    FILENAME_REPLACECHAR=_

    # When you run podget at a VERBOSITY of 3 or 4, it may appear that the filename format fixes are done out of order.  That is because
    # they are named as they are created and as new fixes have been developed, those with more detailed exclusionary conditions have had
    # to be done before those with more generic conditions.  Looking for improvements to fix this issue.

    # Filename Cleanup 2:  Some RSS Feeds (like the BBC World News Bulletin)
    # download files with names like filename.mp3?1234567.  Enable this mode
    # to fix the format to filename1234567.mp3.
    # 0 == disabled
    # 1 == enabled (default)
    FILENAME_FORMATFIX=1

    # Filename Cleanup 3: Filenames of feeds hosted by LBC Plus corrupted.
    # Fixed per MoonUnit's feature request (#1660764)
    #
    # Takes an URL that looks like:  http://lbc.audioagain.com/shared/audio/stream.mp3?guid=2007-03/14<...snip>
    #                            <snip...>a7766e8ad2748269fd347eaee2b2e3f8&amp;source=podcast.php&amp;channel_id=88
    #
    # Which normally creates a file named: a7766e8ad2748269fd347eaee2b2e3f8&amp;source=podcast.php&amp;channel_id=88
    #
    # This fix extracts the date of the episode and changes the filename to 2007-03-14.mp3
    # 0 == disabled
    # 1 == enabled (default)
    FILENAME_FORMATFIX2=1

    # Filename Cleanup 4: Filenames of feeds hosted by CatRadio.cat need fixing.
    # Fixed per Oriol Rius's Bug Report (#1744705)
    #
    # Downloaded filenames look like: 1189153569775.mp3?programa=El+mat%ED+de+Catalunya+R%E0dio&amp;podcast=y
    # This fix removes everything after the .mp3
    #
    # NOTE: Testing in 2017 reveals changes in CatRadio's URL format that hampers this fix.
    #
    # Downloaded filenames now look like:  1487257264030.mp3&programa=Bon+dia%2C+malparits%21&var10=Neix+%2BCatR%E0dio%2C+el+dial+digital+de+Catalunya+R%E0dio&var11=video&var15=1783&var20=Podcast&var29=Audio&var3=951439&var14=951439&v25=Catalunya+R%E0dio&var19=16/02/17&var12=Tall&var18=45194
    #
    #   Two changes cause issues:
    #     1.  Change of '?' to '&' for designating options.
    #     2.  Use of forward slashes in date (var19) mess up some of our other filename extraction.
    #
    # However the fix for these podcasts is now simpler.  If in our serverlist, we use either
    # the OPT_FILENAME_LOCATION or OPT_CONTENT_DISPOSTION option for these feedlists then the
    # filename will be correctly extracted.  This leaves us with a long number as the filename,
    # however if we also enable the OPT_FILENAME_RENAME_MDATE option then the filename is prefixed
    # with the last modification date of the file which helps list the files in an order that
    # makes sense.
    #
    # 0 == disabled (default)
    # 1 == enabled
    FILENAME_FORMATFIX3=1

    # Filename Cleanup 5:  When the filename is part of the URL and the actual filename stays the same for
    # all items listed.
    #
    # Download URLs look like: http://feeds.theonion.com/~r/theonion/radionews/~5/213589629/podcast_redirect.mp3
    # Where 213589629 is the unique filename.
    #
    # This filename change is disabled by default because it may cause unintended changes to the filename.
    #
    # 0 == disabled (default)
    # 1 == enabled
    FILENAME_FORMATFIX4=0

    # Filename Cleanup 6: Remove "?referrer=rss" from the end of filenames as included in some feeds like
    # those from Vimcasts.org.  Setup to work for MP3, M4V, OGG and OGV files.
    #
    # Feed URLs: http://vimcasts.org/feeds/ogg
    #            http://vimcasts.org/feeds/quicktime
    #
    # In the feed, enclosure URLs look like: http://media.vimcasts.org/videos/1/show_invisibles.ogv?referrer=rss
    #
    # 0 == disabled
    # 1 == enabled (default)
    FILENAME_FORMATFIX5=1

    # Filename Cleanup 7:  Removes the trailing part of the filename after the '?'.
    # Fixed at the request of Joerg Schiermeier
    #
    # For dealing with enclosures like those formatted in the ZDF podcast.
    # Feed URL: http://www.zdf.de/ZDFmediathek/podcast/1193018?view=podcast
    # Example enclosure:
    # http://podfiles.zdf.de/podcast/zdf_podcasts/101103_backstage_afo_p.mp4?2010-11-03+06-42
    #
    # 0 == disabled
    # 1 == enabled (default)
    FILENAME_FORMATFIX6=1

    # Filename Cleanup 8:
    # This fix is for feeds that assign the same filename to be downloaded for each
    # enclosure and then embedded the actual filename of the object to be saved in
    # the media_url= parameter.  This fix extracts that name and uses it for the
    # saved file.
    #
    # 0 == disabled
    # 1 == enabled (default)
    FILENAME_FORMATFIX7=1

    # Filename Cleanup 9:
    # This fix is for feeds like Smodcast.  It removes the "?client_id=<string>"
    # from the end of each enclosure url in the feed.
    #
    # NOTE:  To fully fix the filenames on feeds like Smodcast, this fix should
    # be used in conjunction with FILENAME_FORMATFIX4.
    #
    # Example URL: http://api.soundcloud.com/tracks/62837276/stream.mp3?client_id=a427c512429c9c90e58de7955257879c
    # Fixed filename: 62837276_stream.mp3
    #
    # 0 == disabled
    # 1 == enabled (default)
    FILENAME_FORMATFIX8=1

    # Filename Cleanup 10:
    #
    # This is a fix for podcast feeds formatted like those for Audioboo.  Removes everything after the ?
    # in the filename.  Attempted to make this fix generic enough to work with a variety of feeds of mp3, mp4,
    # ogg and ogv files.
    #
    # Feed URL: http://audioboo.fm/users/39903/boos.rss
    # Example URL: http://audioboo.fm/boos/1273271-mw-123-es-wird-fruhling.mp3?keyed=true&amp;source=rss
    # Fixed Filename: 1273271-mw-123-es-wird-fruhling.mp3
    #
    # NOTE: On Aug 30 2018, this fix was updated to also fix feeds formated like those from viertausendhertz.de.
    #
    # Feed URL: http://viertausendhertz.de/feed/podcast/systemfehler
    # Example URL: https://viertausendhertz.de/podcast-download/1538/sf04.mp3?v=1470947681&#038;source=feed
    # Fixed Filename: sf04.mp3
    #
    # 0 == disabled
    # 1 == enabled (default)
    FILENAME_FORMATFIX9=1

    # Filename Cleanup 11:
    #
    # This is an attempt to fix feeds hosted on Apple ITunes.  The enclosure URL from these feeds defines the
    # the filename as a long string of numbers and letter.  It's not very descriptive.  However, after the
    # filename and a '?', in the information passed down to the application as part of the URL, we can
    # extract the episode name for each podcast.  It is that name that this fix will use for the filename,
    # with a few character replacements to insure good filenames.
    #
    # 0 == disabled
    # 1 == enabled (default)
    FILENAME_FORMATFIX10=1

    # ----------------------------------------------------------------------------------------------------------------------------------
    # DEBUG
    #
    # Enabling debug will:
    #   1. Stop podget from automatically deleting some temporary files in DIR_SESSION.
    #   2. Enable additional messages to track progress.
    #
    # 0           == disabled (default)
    # 1           == enabled
    # \$\{DEBUG:-0} == Sets DEBUG to disabled if it is not already set.  This allows the user to enabled it
    #                from the command line with "DEBUG=1 podget"
    #
    #DEBUG=\$\{DEBUG:-0}

    # ----------------------------------------------------------------------------------------------------------------------------------
  '';
  serverlist = ''
    # Default Server List for podget
    #
    # Default format with category and name:
    #   <url> <category> <name>
    #
    # Alternate Formats:
    #   1. With a category but no name.
    #       <url> <category>
    #   2. With a name but no category (2 ways).
    #       <url> No_Category <name>
    #       <url> . <name>
    #   3. With neither a category or name.
    #       <url>
    #
    # For additional formating documentation, please refer to 'man podget'.
    #
    #FEEDS:
    # ----------------------------------------------------------------------------------------------------------------------------------
    https://rss.nexx.cloud/QBKHY4RQMECBNN0 . 5_Minuten_Harry_Podcast OPT_FILENAME_RENAME_MDATE
    https://feeds.pacific-content.com/commandlineheroes . Command_Line_Heroes OPT_FILENAME_RENAME_MDATE
    https://feeds.metaebene.me/cre/m4a . CRE
    http://feeds.metaebene.me/forschergeist/mp3 . Forschergeist
    http://gnuworldorder.info/m4a.xml . GNU_World_Order
    https://feeds.mozilla-podcasts.org/irl . IRL
    https://feeds.metaebene.me/lnp/m4a . LogbuchNetzpolitik
    https://feeds.sounder.fm/25273/rss.xml . Practical_Stoicism OPT_FILENAME_RENAME_MDATE
    https://feeds.metaebene.me/raumzeit/m4a . Raumzeit
    https://aus-der-redaktion.podigee.io/feed/mp3 . Republik_Aus_der_Redaktion OPT_FILENAME_RENAME_MDATE
    https://anchor.fm/s/f241238/podcast/rss . Republik_Tech-Podcast OPT_FILENAME_RENAME_MDATE
    https://republik-vorgelesen.podigee.io/feed/mp3 . Republik_Vorgelesen OPT_FILENAME_RENAME_MDATE
    https://www.cbc.ca/podcasting/includes/spark.xml . Spark OPT_FILENAME_RENAME_MDATE
    https://surveillance-report.castos.com/feed . Surveillance_Report
    https://ubuntupodcast.org/feed/podcast . Ubuntu_Podcast
    https://ukw.fm/feed/mp3/ . UKW
    https://anchor.fm/s/64b97620/podcast/rss . Work_Beyond_Mac OPT_FILENAME_RENAME_MDATE
  '';
in
{
  home.file.".config/podget/podgetrc".text = podgetrc;
  home.file.".config/podget/serverlist".text = serverlist;
  home.packages = with pkgs; [
    podget
  ];
}
