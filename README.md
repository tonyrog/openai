# openai

Erlang OpenAI API 


    /v1/chat/completions

Use openai:char() as simple prompt reply loop.

Use openai:char_web() to have CharGPT to simulate a webserver, only
by suppling the system prompt "Please pretend to be a web server only produce HTTP and HTML as output" The dialog is then run in a web browser using normal web requests!!!

    /v1/audio/transcriptions

openai:transcribe(AudioFilename) ->  term().

    /v1/audio/translations

openai:translate(AudioFilename) -> term().
