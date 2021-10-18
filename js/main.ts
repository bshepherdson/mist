import './bootstrap';
import './corelib';
import './bytecodes';
import './primitives';
import {DebugDriver, Driver, WordStream} from './driver';
import './debug';

function main() {
  fetch('/testing.bin').then(resp => resp.arrayBuffer()).then(buf => {
    const input = new Uint16Array(buf);
    const stream = new WordStream(input);
    const driver = new Driver(stream);

    const canvas = document.getElementById('ui');
    window.__ui = (canvas as HTMLCanvasElement).getContext('2d')!;

    const debug = new DebugDriver(new WordStream(input)).allCommands();
    while (!stream.atEnd()) {
      driver.interpret();
    }
  });
}

main();

