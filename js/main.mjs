import './bootstrap.mjs';
import './corelib.mjs';
import './bytecodes.mjs';
import './primitives.mjs';
import {Driver, WordStream} from './driver.mjs';

function main() {
  fetch('/testing.bin').then(resp => resp.arrayBuffer()).then(buf => {
    const input = new Uint16Array(buf);
    const stream = new WordStream(input);
    const driver = new Driver(stream);

    debugger;
    while (!stream.atEnd()) {
      driver.interpret();
    }
  });
}

main();

