import './bootstrap';
import './corelib';
import './bytecodes';
import './primitives';
import {DebugDriver, Driver, WordStream} from './driver';

function main() {
  fetch('/testing.bin').then(resp => resp.arrayBuffer()).then(buf => {
    const input = new Uint16Array(buf);
    const stream = new WordStream(input);
    const driver = new Driver(stream);

    const debug = new DebugDriver(new WordStream(input)).allCommands();
    debugger;
    while (!stream.atEnd()) {
      driver.interpret();
    }
  });
}

main();

