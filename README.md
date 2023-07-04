# openai

Erlang OpenAI API 


    /v1/chat/completions

Use **openai:chat()** as simple prompt reply loop.

Use **openai:chat_web()** to have ChatGPT simulate a webserver, using the system prompt "Please pretend to be a web server only producing HTTP and HTML as output" The dialog is then run in a web browser using normal web requests!!!

    /v1/audio/transcriptions

openai:transcribe(AudioFilename) ->  term().

    /v1/audio/translations

openai:translate(AudioFilename) -> term().
