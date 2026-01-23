GET http://localhost:8080/submit-form
HTTP/1.1 200
[Asserts]
xpath "normalize-space(//head/title)" startsWith "Wrench"
xpath "normalize-space(//script[@id='variants-data'])" contains "\"factorial\""

POST http://localhost:8080/submit
[Form]
name: Bob
asm: .text \n _start: \n halt \n
config: {limit: 100, memory_size: 128}
comment: "This is a comment"
variant:
isa: acc32
HTTP/1.1 301
[Captures]
report_url: header "Location"
track_id: cookie "track_id"

GET http://localhost:8080{{report_url}}
HTTP/1.1 200
[Captures]
debug_link: xpath "normalize-space(//span[@id='debug-link'])"
[Asserts]
header "Set-Cookie" startsWith "track_id={{track_id}}"
xpath "normalize-space(//head/title)" startsWith "Simulation Results"
xpath "normalize-space(//span[@id='debug-link'])" startsWith "/debug/"
xpath "normalize-space(//pre[@id='simulation-config-text-element'])" contains "limit: 100, memory_size: 128"
xpath "normalize-space(//pre[@id='status-text-element'])" contains "ExitSuccess"
xpath "normalize-space(//pre[@id='simulation-log-text-element'])" contains "Executed Instruction"
xpath "normalize-space(//pre[@id='simulation-log-text-element'])" contains "Halt @_start"
xpath "normalize-space(//pre[@id='dump-text-element'])" contains "_start"
xpath "normalize-space(//pre[@id='dump-text-element'])" contains "mem[0..0]: Halt"

GET http://localhost:8080{{debug_link}}
HTTP/1.1 200
[Asserts]
xpath "normalize-space(//head/title)" startsWith "Wrench Debugger"
xpath "normalize-space(//span[contains(., 'debug view for')])" contains "debug view for Bob (- / acc32)"
xpath "normalize-space(//script[@id='exec-log-data'])" contains "\"elInitial\""

GET http://localhost:8080/submissions
HTTP/1.1 200
[Asserts]
xpath "normalize-space(//head/title)" startsWith "My Submissions"
xpath "normalize-space(//h2)" contains "my_submissions"
xpath "//div[@id='submissions-list']" exists

GET http://localhost:8080/examples
HTTP/1.1 200
[Asserts]
xpath "normalize-space(//head/title)" startsWith "Wrench Examples"
xpath "normalize-space(//h2)" contains "curated_examples"
xpath "//div[@id='examples-status']" exists
