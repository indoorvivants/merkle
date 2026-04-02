test:
	scala-cli --power test . --cross

publish-snapshot:
	scala-cli config publish.credentials central.sonatype.com env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	scala-cli config publish.credentials ossrh-staging-api.central.sonatype.com env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	scala-cli publish --cross . --signer none

publish:
	scala-cli config publish.credentials central.sonatype.com env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	scala-cli config publish.credentials ossrh-staging-api.central.sonatype.com env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	./.github/workflows/import-gpg.sh
	scala-cli publish --cross . --signer gpg --gpg-key 9D8EF0F74E5D78A3
