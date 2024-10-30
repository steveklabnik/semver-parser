// Copyright 2024 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#![no_main]

use libfuzzer_sys::fuzz_target;
use semver_parser::{lexer::Lexer, parser::Parser, Compat};

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = format!("{}\n{}", "", s).parse::<proc_macro2::TokenStream>();

        // Fuzz Lexer
        let mut lexer = Lexer::new(s);
        while let Some(token) = lexer.next() {
            match token {
                Err(_) => break,
                Ok(_) => {}
            }
        }

        // Fuzz Parser
        if let Ok(mut parser) = Parser::new(s) {
            let _ = parser.version();
        }

        // Fuzz Compat
        for compat in &[Compat::Cargo, Compat::Npm] {
            let _ = format!("{:?}", compat);
        }
    }
});
