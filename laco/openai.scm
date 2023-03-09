;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2023
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Laco is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License published
;;  by the Free Software Foundation, either version 3 of the License,
;;  or (at your option) any later version.

;;  Laco is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program. If not, see <http://www.gnu.org/licenses/>.

(define-module (laco openai)
  #:export (ai-check))

(define *api-url* "https://api.openai.com/v1/chat/completions")
(define *data* "
{
\"model\": \"gpt-3.5-turbo\",
\"messages\": [{\"role\": \"system\", \"content\": ~s}],
\"temperature\": 0.7
}")
(define *json-handle* "jq -r '.choices[].message.content'")

(define (openai-send key prompt)
  (define (gen-data)
    (format #f *data* prompt))
  (let* ((type "-H 'Content-Type: application/json'")
         (cmd (format #f "curl -s ~a ~a -H 'Authorization: Bearer ~a' -d '~a' | ~a"
                      *api-url* type key (gen-data) *json-handle*)))
    (system cmd)))

(define (ai-check src key)
  (openai-send
   key
   (format #f "This r7rs Scheme code is failed to compile, give advices, only show fixed code: ~s" src)))
