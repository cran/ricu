# cli progress

    Code
      with_progress(do_stuff(pb), pb)
    Message <progress_header>
      * a
    Message <progress_body>
        ( ) hello from index 1
        ( ) hello from index 2
        ( ) hello from index 3
    Message <progress_header>
      * abcd abcd abcd abcd
    Message <progress_body>
        ( ) hello from index 4
        ( ) hello from index 5
        ( ) hello from index 6
    Message <progress_header>
      * abcdefg abcdefg abcdefg abcdefg abcdefg abcdefg abcdefg
    Message <progress_body>
        ( ) hello from index 7
        ( ) hello from index 8
        ( ) hello from index 9
    Message <progress_header>
      * abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij
        abcdefghij abcdefghij abcdefghij abcdefghij
    Message <progress_body>
        ( ) hello from index 10
    Message <cliMessage>
      --------------------------------------------------------------------------------
    Output
      NULL

---

    Code
      with_progress(do_stuff(pb), pb)
    Message <progress_header>
      * a
    Message <progress_body>
        ( ) hello from index 1
        ( ) hello from index 2
        ( ) hello from index 3
    Message <progress_header>
      * abcd abcd abcd abcd
    Message <progress_body>
        ( ) hello from index 4
        ( ) hello from index 5
        ( ) hello from index 6
    Message <progress_header>
      * abcdefg abcdefg abcdefg abcdefg abcdefg abcdefg abcdefg
    Message <progress_body>
        ( ) hello from index 7
        ( ) hello from index 8
        ( ) hello from index 9
    Message <progress_header>
      * abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij
        abcdefghij abcdefghij abcdefghij abcdefghij
    Message <progress_body>
        ( ) hello from index 10
    Message <cliMessage>
      --------------------------------------------------------------------------------
    Output
      NULL

